const n = `package main;

use packages_basic::Lib;

fn main() {
    println(Lib::color_to_int(Lib::Color::Red).to_string())
}
`;
export {
  n as default
};
