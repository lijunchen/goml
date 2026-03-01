const n = `fn main() -> unit {
    let c: char = '\\u0041';
    string_println(c.to_string());

    let d: char = 'b';
    let out = match d {
        'a' => "A",
        'b' => "B",
        _ => "?",
    };
    string_println(out);
}
`;
export {
  n as default
};
