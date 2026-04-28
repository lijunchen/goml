package main

import (
    _goml_fmt "fmt"
    "os"
    "io"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func go_error_to_string(value GoError) string {
    return value.Error()
}

type Result__Reader__GoError interface {
    isResult__Reader__GoError()
}

type Ok struct {
    _0 Reader
}

func (_ Ok) isResult__Reader__GoError() {}

type Err struct {
    _0 GoError
}

func (_ Err) isResult__Reader__GoError() {}

type GoError = error

type Reader = io.Reader

func open_reader_ffi_wrap(p0 string) Result__Reader__GoError {
    var ffi_value_0 Reader
    var ffi_err GoError
    ffi_value_0, ffi_err = os.Open(p0)
    if ffi_err != nil {
        return Err{
            _0: ffi_err,
        }
    }
    return Ok{
        _0: ffi_value_0,
    }
}

func describe(path__0 string) string {
    var retv5 string
    var mtmp0 Result__Reader__GoError = open_reader_ffi_wrap(path__0)
    var jp7 string
    switch mtmp0.(type) {
    case Ok:
        var x1 Reader = mtmp0.(Ok)._0
        var reader__1 Reader = x1
        var t8 string = _goml_fmt.Sprintf("type=%T", reader__1)
        jp7 = t8
    case Err:
        var x2 GoError = mtmp0.(Err)._0
        var err__2 GoError = x2
        var t9 string = go_error_to_string(err__2)
        var t10 string = "err=" + t9
        jp7 = t10
    default:
        panic("non-exhaustive match")
    }
    retv5 = jp7
    return retv5
}

func main0() struct{} {
    var t12 string = describe("/etc/hosts")
    println__T_string(t12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
