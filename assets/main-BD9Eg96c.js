const e = `extern "go" "os" "*File" type File

#[go_error_last]
extern "go" "os" "Open" open_file(path: string) -> Result[File, GoError]

extern "go" "os" "(*File).Name" file_name(file: File) -> string

#[go_error]
extern "go" "os" "(*File).Close" close_file(file: File) -> Result[unit, GoError]

fn describe(path: string) -> Result[string, GoError] {
    let file = open_file(path)?;
    let name = file_name(file);
    close_file(file)?;
    Result::Ok("ok=" + name)
}

fn show(res: Result[string, GoError]) -> string {
    match res {
        Result::Ok(value) => value,
        Result::Err(err) => "err=" + err.to_string(),
    }
}

fn main() -> unit {
    println(show(describe("/etc/hosts")));
    println(show(describe("/definitely/missing")));
}
`;
export {
  e as default
};
