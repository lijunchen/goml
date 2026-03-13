const n = `package main;

fn main() -> unit {
    let mut x = 0;
    let inc = || {
        x = x + 1;
        x
    };
    let _ = inc();
    string_println(int32_to_string(x));
}
`;
export {
  n as default
};
