const n = `fn main() {
    let _ = ();
    let a = (true, false, (true, false));
    let b = match a {
        (x, y, (z, w)) => w
    };
    let _ = print(b);
    ()
}
`;
export {
  n as default
};
