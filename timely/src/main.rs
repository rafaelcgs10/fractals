extern crate timely;

use std::collections::HashMap;

use timely::communication::message::RefOrMut;
use timely::dataflow::channels::pact::Pipeline;
use timely::dataflow::operators::*;
use timely::dataflow::{InputHandle, ProbeHandle, Scope, Stream};
use timely::Data;

const WIDTH: u32 = 1800 * 3;
const HEIGHT: u32 = 1200 * 3;
const ITERATIONS: u32 = 400;

fn main() {
    timely::execute_from_args(std::env::args(), move |worker| {
        let mut input = InputHandle::new();
        let mut probe = ProbeHandle::new();
        let index = worker.index();

        worker.dataflow(|scope| {
            let stream = scope.input_from(&mut input);

            let result = scope.iterative::<u32, _, _>(|subscope| {
                let (loop_handle, loop_stream) = subscope.loop_variable(1);

                let (end, continuation) = stream
                    .enter(subscope)
                    .concat(&loop_stream)
                    .exchange(|(_c, _z, (x, y), iter)| (*x + *y + *iter) as u64)
                    .map(|((r, i), (zr, zi), pos, iter)| {
                        let c = num::complex::Complex::new(r, i);
                        let mut z0 = num::complex::Complex::new(zr, zi);
                        let mut count = 0;
                        for i in 0..ITERATIONS / 4 {
                            z0 = z0 * z0 + c;
                            count = i;
                            if z0.norm_sqr() >= 4.0 {
                                break;
                            }
                        }
                        ((r, i), (z0.re, z0.im), pos, iter + count)
                    })
                    .branch(|_t, (_c, (zr, zi), _pos, iter)| {
                        let z0 = num::complex::Complex::new(*zr, *zi);
                        let result = z0.norm_sqr() < 4.0 && *iter < ITERATIONS;
                        result
                    });

                continuation.connect_loop(loop_handle);

                let result = end.map(|(_c, _z, pos, iter)| (pos, iter + 1)).leave();
                result
            });

            stream.probe_with(&mut probe);

            result
                .exchange(|((_x, _y), _iter)| 0)
                .accumulate_matrix(
                    vec![vec![(0f64, 0f64, 0f64); WIDTH as usize]; HEIGHT as usize],
                    |matrix, data| {
                        for &((x, y), iter) in data.iter() {
                            matrix[y as usize][x as usize] = calc_color(iter, ITERATIONS);
                        }
                    },
                )
                .inspect(move |matrix| {
                    print_ppm_header(WIDTH, HEIGHT);
                    print_ppm_mandel(matrix);
                });
        });

        input.advance_to(0);
        for round in 0..1 {
            if index == 0 {
                for y in 0..HEIGHT {
                    for x in 0..WIDTH {
                        let (r, i) = normalize_to_plane(WIDTH, HEIGHT, x, y);
                        input.send(((r, i), (0.0, 0.0), (x, y), 0));
                    }
                }
            }
            input.advance_to(round + 1);
            while probe.less_than(input.time()) {
                worker.step();
            }
        }
    })
    .unwrap();
}

fn normalize_to_plane(width: u32, height: u32, x: u32, y: u32) -> (f64, f64) {
    (
        3.0 / (width as f64) * (x as f64) - 2.0,
        2.0 / (height as f64) * (y as f64) - 1.0,
    )
}

fn calc_color(iter: u32, iterations: u32) -> (f64, f64, f64) {
    if iter == iterations {
        return (0.0, 0.0, 0.0);
    }
    let normalized = (iter as f64) / (iterations as f64);
    let red = if iter as f64 > (iterations as f64) * 0.8 {
        normalized
    } else {
        normalized.powf(0.3)
    };
    let green = if iter as f64 > (iterations as f64) * 0.8 {
        normalized
    } else {
        normalized.powf(0.2)
    };
    let blue = if iter as f64 > (iterations as f64) * 0.8 {
        normalized
    } else {
        normalized.powf(0.1)
    };

    (red, green, blue)
}

fn to_256(n: f64) -> u32 {
    ((n * 255.0).floor()) as u32
}

fn print_ppm_mandel(matrix: &Vec<Vec<(f64, f64, f64)>>) {
    for line in matrix.iter() {
        for element in line.iter() {
            let (r, g, b) = *element;
            print!("{} {} {} ", to_256(r), to_256(g), to_256(b));
        }
        println!("");
    }
}

fn print_ppm_header(width: u32, height: u32) {
    println!("P3");
    println!("{} {}", width, height);
    println!("255");
}

pub trait AccumulateMatrix<G: Scope, D: Data> {
    fn accumulate_matrix<A: Data>(
        &self,
        default: Vec<Vec<A>>,
        logic: impl Fn(&mut Vec<Vec<A>>, RefOrMut<Vec<D>>) + 'static,
    ) -> Stream<G, Vec<Vec<A>>>;
}

impl<G: Scope, D: Data> AccumulateMatrix<G, D> for Stream<G, D> {
    fn accumulate_matrix<A: Data>(
        &self,
        default: Vec<Vec<A>>,
        logic: impl Fn(&mut Vec<Vec<A>>, RefOrMut<Vec<D>>) + 'static,
    ) -> Stream<G, Vec<Vec<A>>> {
        let mut accums = HashMap::new();
        self.unary_notify(
            Pipeline,
            "AccumulateVec",
            vec![],
            move |input, output, notificator| {
                input.for_each(|time, data| {
                    logic(
                        &mut accums
                            .entry(time.time().clone())
                            .or_insert_with(|| default.clone()),
                        data,
                    );
                    notificator.notify_at(time.retain());
                });

                notificator.for_each(|time, _, _| {
                    if let Some(accum) = accums.remove(&time) {
                        output.session(&time).give(accum);
                    }
                });
            },
        )
    }
}
