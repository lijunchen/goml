const n = `fn show_int(label: string, value: int32) {
    let _ = string_println(label + int32_to_string(value));
    ()
}

fn show_bool(label: string, value: bool) {
    let _ = string_println(label + bool_to_string(value));
    ()
}

fn main() {
    let base = 10;
    let sum = base + 5;
    let diff = sum - 3;
    let prod = diff * 2;
    let quot = prod / 4;

    let _ = show_int("sum=", sum);
    let _ = show_int("diff=", diff);
    let _ = show_int("prod=", prod);
    let _ = show_int("quot=", quot);

    let and_result = true && false;
    let or_result = true || false;
    let not_result = !false;
    let mixed = !and_result
        && match sum + prod * base - prod / 2 {
            0 => false,
            _ => true,
        }
        || !match diff - quot + base - sum / 2 {
            0 => false,
            _ => true,
        };

    let _ = show_bool("and=", and_result);
    let _ = show_bool("or=", or_result);
    let _ = show_bool("not=", not_result);
    let _ = show_bool("mixed=", mixed);

    ()
}
`;
export {
  n as default
};
