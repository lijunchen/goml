const n = `fn main() -> unit {
    let mut x = 0;
    let get = || x;
    x = 41;
    println(get().to_string());
}
`;
export {
  n as default
};
