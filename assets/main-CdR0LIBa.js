const n = `fn main() -> unit {
    // Create a new empty Vec with type annotation
    let v: Vec[int32] = vec_new();
    
    // Push elements
    let v = vec_push(v, 10);
    let v = vec_push(v, 20);
    let v = vec_push(v, 30);
    
    // Get elements
    let first = vec_get(v, 0);
    let second = vec_get(v, 1);
    let third = vec_get(v, 2);
    
    // Get length
    let len = vec_len(v);
    
    // Print results
    let _ = string_println(int32_to_string(first));
    let _ = string_println(int32_to_string(second));
    let _ = string_println(int32_to_string(third));
    let _ = string_println(int32_to_string(len));
    ()
}
`;
export {
  n as default
};
