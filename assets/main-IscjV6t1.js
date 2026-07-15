const n = `fn child(signal: Ref[int32]) -> unit {
    signal.set(1)
}

fn main() -> unit {
    let signal = Ref::new(0);
    go || { child(signal) };
    while signal.get() < 1 {
        ()
    };
    println("main");
}
`;
export {
  n as default
};
