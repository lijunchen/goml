package main

import (
    "fmt"
    "time"
    "os"
    "io"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type GoError = error

type Reader = io.Reader

func rfc3339_ffi_wrap() string {
    return time.RFC3339
}

func stdin_ffi_wrap() Reader {
    return os.Stdin
}

func main0() struct{} {
    var t3 string = rfc3339_ffi_wrap()
    println__T_string(t3)
    var t4 Reader = stdin_ffi_wrap()
    var t5 string = fmt.Sprintf("stdin=%T", t4)
    println__T_string(t5)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
