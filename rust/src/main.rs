const WINDOW_WIDTH: i32 = 1800;
const WINDOW_HEIGHT: i32 = 1200;
const ITERATIONS: i32 = 200;

fn main() {
    let matrix = points(WINDOW_WIDTH, WINDOW_HEIGHT);
    print_ppm_header(WINDOW_WIDTH, WINDOW_HEIGHT);
    print_ppm_mandel(matrix);
}

fn mandel_interations(c: num::Complex<f64>) -> i32 {
    let mut z = num::complex::Complex::new(0.0, 0.0);
    for i in 0..ITERATIONS {
        z = z * z + c;
        if z.norm_sqr() > 4.0 {
            return i + 1;
        }
    }
    return ITERATIONS;
}

fn normalize_to_plane(x: i32, y: i32) -> (f64, f64) {
    (
        3.0 / (WINDOW_WIDTH as f64) * (x as f64) - 2.0,
        2.0 / (WINDOW_HEIGHT as f64) * (y as f64) - 1.0,
    )
}

fn calc_color(iter: i32) -> (f64, f64, f64) {
    if iter == ITERATIONS {
        return (0.0, 0.0, 0.0);
    }
    let normalized = (iter as f64) / (ITERATIONS as f64);
    let red = if iter as f64 > (ITERATIONS as f64) * 0.8 {
        normalized
    } else {
        normalized.powf(0.3)
    };
    let green = if iter as f64 > (ITERATIONS as f64) * 0.8 {
        normalized
    } else {
        normalized.powf(0.2)
    };
    let blue = if iter as f64 > (ITERATIONS as f64) * 0.8 {
        normalized
    } else {
        normalized.powf(0.1)
    };

    (red, green, blue)
}

fn points(width: i32, height: i32) -> Vec<Vec<(f64, f64, f64)>> {
    let mut matrix = vec![vec![(0.0f64, 0.0f64, 0.0f64); width as usize]; height as usize];

    for j in 0..height as usize {
        for i in 0..width as usize {
            let (x, y) = normalize_to_plane(i as i32, j as i32);
            let c = num::complex::Complex::new(x, y);

            let iterations = mandel_interations(c);
            let color = calc_color(iterations);
            matrix[j][i] = color;
        }
    }
    matrix
}

fn to_256(n: f64) -> i32 {
    ((n * 255.0).floor()) as i32
}

fn print_ppm_mandel(matrix: Vec<Vec<(f64, f64, f64)>>) {
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
