const n = `fn show32(label: string, value: float32) {
    let message = label + float32_to_string(value);
    let _ = string_println(message);
    ()
}

fn show64(label: string, value: float64) {
    let message = label + float64_to_string(value);
    let _ = string_println(message);
    ()
}

fn lerp32(a: float32, b: float32, weight: float32) -> float32 {
    let delta = b - a;
    a + delta * weight
}

fn midpoint_energy(x: float64, y: float64) -> float64 {
    let sum = x * x + y * y;
    sum / 2.0f64
}

fn main() {
    let start32: float32 = 1.25f32;
    let end32: float32 = 5.75f32;
    let half: float32 = 0.5f32;
    let scale: float32 = 2.0f32;

    let mid32 = lerp32(start32, end32, half);
    let neg_end32 = -end32;
    let ratio32 = end32 / scale;
    let less32 = start32 < end32;

    let dx: float64 = 6.5f64;
    let dy: float64 = 3.5f64;
    let quarter: float64 = 0.25f64;
    let energy: float64 = midpoint_energy(dx, dy);
    let neg_dx = -dx;
    let adjusted: float64 = energy + dy - dx * quarter;
    let threshold: float64 = 4.0f64;
    let less64 = adjusted < threshold;

    show32("mid32=", mid32);
    show32("neg_end32=", neg_end32);
    show32("ratio32=", ratio32);
    let _ = string_println("less32=" + bool_to_string(less32));

    show64("energy=", energy);
    show64("neg_dx=", neg_dx);
    show64("adjusted=", adjusted);
    let _ = string_println("less64=" + bool_to_string(less64));
}
`;
export {
  n as default
};
