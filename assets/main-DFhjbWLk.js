const n = `fn a_value() -> int8 {
    90i8
}

fn b_value() -> int8 {
    -20i8
}

fn c_value() -> int8 {
    3i8
}

fn show_int8(label: string, value: int8) {
    let _ = string_println(label + int8_to_string(value));
    ()
}

fn show_bool(label: string, value: bool) {
    let _ = string_println(label + bool_to_string(value));
    ()
}

fn main() {
    let a = a_value();
    let b = b_value();
    let c = c_value();

    let sum = a + b;
    let diff = a - c;
    let prod = b * c;
    let quot = a / c;
    let neg = -b;
    let less = b < a;

    let _ = show_int8("a=", a);
    let _ = show_int8("b=", b);
    let _ = show_int8("c=", c);
    let _ = show_int8("sum=", sum);
    let _ = show_int8("diff=", diff);
    let _ = show_int8("prod=", prod);
    let _ = show_int8("quot=", quot);
    let _ = show_int8("neg=", neg);
    let _ = show_bool("b<a=", less);

    ()
}
`;
export {
  n as default
};
