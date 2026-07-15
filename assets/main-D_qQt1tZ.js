const n = `use std::fs;
use std::io;

fn show_read(res: Result[string, string]) -> string {
    match res {
        Result::Ok(value) => value,
        Result::Err(err) => "err " + err,
    }
}

fn show_dir(res: Result[Vec[string], string]) -> string {
    match res {
        Result::Ok(names) => (names.len() > 0).to_string(),
        Result::Err(err) => "err " + err,
    }
}

fn main() -> unit {
    let _ = fs::write_file("goml-std-test.txt", "std-ok");
    io::println(show_read(fs::read_file("goml-std-test.txt")));
    io::println(fs::exists("goml-std-test.txt").to_string());
    io::println(show_dir(fs::read_dir(".")));
}
`;
export {
  n as default
};
