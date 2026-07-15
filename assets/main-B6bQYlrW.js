const n = `fn main() -> unit {
    let c: char = '\\u0041';
    println(c.to_string());

    let d: char = 'b';
    let out = match d {
        'a' => "A",
        'b' => "B",
        _ => "?",
    };
    println(out);
}
`;
export {
  n as default
};
