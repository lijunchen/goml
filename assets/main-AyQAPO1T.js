const n = `struct Point {
  x: int32,
  y: int32
}

fn main() {
  let p0 = Point { x: 0, y: 0 };
  let Point { x, y } = p0;
  let _ = x + y;
  ()
}
`;
export {
  n as default
};
