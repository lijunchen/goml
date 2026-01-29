const t = `fn main() {
    let _ = string_print(unit_to_string(()));
    let _ = string_print(bool_to_string(true));
    let _ = string_print(bool_to_string(false));
    let _ = string_print(int32_to_string(123));
    ()
}
`;
export {
  t as default
};
