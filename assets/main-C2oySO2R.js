const n = `fn main() -> unit {
    let value: int32 = 42;
    let text = value.to_string();
    let _ = string_println(text);
    ()
}
`;
export {
  n as default
};
