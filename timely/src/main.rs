extern crate timely;

use timely::dataflow::operators::aggregation::Aggregate;
use timely::dataflow::operators::capture::Extract;
use timely::dataflow::operators::*;
use timely::dataflow::{InputHandle, ProbeHandle, Scope};

fn main() {
    const WIDTH: i32 = 0o6;
    const HEIGHT: i32 = 9;
    // let mut matrix = vec![vec![(0.0f64, 0.0f64, 0.0f64); WIDTH as usize]; HEIGHT as usize];

    timely::execute_from_args(std::env::args(), move |worker| {
        let mut input = InputHandle::new();
        let mut probe = ProbeHandle::new();

        worker.dataflow(|scope| {
            let stream = scope.input_from(&mut input);
            let result = scope.iterative::<u32, _, _>(|subscope| {
                let (loop_handle, loop_stream) = subscope.loop_variable(1);

                let (goes_infinity, continuation) = stream
                    .enter(subscope)
                    .concat(&loop_stream)
                    .map(|(c, z, pos, iter)| (c, z * z + c, pos, iter + 1))
                    .branch(
                        |_t,
                         (_c, z, _pos, _iter): &(
                            num::complex::Complex<f64>,
                            num::complex::Complex<f64>,
                            (i32, i32),
                            i32,
                        )| { z.norm_sqr() < 2.0 },
                    );

                let (last_iteration, continuation2) = continuation.branch_when(|t| t.inner < 40);

                continuation2.connect_loop(loop_handle);

                let result = goes_infinity
                    .concat(&last_iteration)
                    .map(|(_c, _z, pos, iter)| (pos, iter))
                    .leave();
                result
            });

            stream.probe_with(&mut probe);

            // println!("result: {:?}", final_result.extract());
        });

        let z = num::complex::Complex::new(0.0, 0.0);
        input.advance_to(0);
        for round in 0..1 {
            for x in 0..WIDTH {
                for y in 0..HEIGHT {
                    let (r, i) = normalize_to_plane(WIDTH, HEIGHT, x, y);
                    let c = num::complex::Complex::new(r, i);
                    input.send((c, z, (x, y), 0));
                }
            }
            input.advance_to(round + 1);
            while probe.less_than(input.time()) {
                worker.step();
            }
        }
    })
    .unwrap();

    // print_ppm_header(WIDTH, HEIGHT);
    // print_ppm_mandel(&mut matrix);
}

fn normalize_to_plane(width: i32, height: i32, x: i32, y: i32) -> (f64, f64) {
    (
        3.0 / (width as f64) * (x as f64) - 2.0,
        2.0 / (height as f64) * (y as f64) - 1.0,
    )
}

fn calc_color(iter: i32, iterations: i32) -> (f64, f64, f64) {
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

fn to_256(n: f64) -> i32 {
    ((n * 255.0).floor()) as i32
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

fn print_ppm_header(width: i32, height: i32) {
    println!("P3");
    println!("{} {}", width, height);
    println!("255");
}
