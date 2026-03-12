package main

import (
    "fmt"
    "os"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func go_error_to_string(value GoError) string {
    return value.Error()
}

type Result__File__GoError interface {
    isResult__File__GoError()
}

type Result__File__GoError_Ok struct {
    _0 File
}

func (_ Result__File__GoError_Ok) isResult__File__GoError() {}

type Result__File__GoError_Err struct {
    _0 GoError
}

func (_ Result__File__GoError_Err) isResult__File__GoError() {}

type Result__unit__GoError interface {
    isResult__unit__GoError()
}

type Result__unit__GoError_Ok struct {
    _0 struct{}
}

func (_ Result__unit__GoError_Ok) isResult__unit__GoError() {}

type Result__unit__GoError_Err struct {
    _0 GoError
}

func (_ Result__unit__GoError_Err) isResult__unit__GoError() {}

type GoError = error

type File = *os.File

func open_file_ffi_wrap(p0 string) Result__File__GoError {
    var ffi_value_0 File
    var ffi_err GoError
    ffi_value_0, ffi_err = os.Open(p0)
    if ffi_err != nil {
        return Result__File__GoError_Err{
            _0: ffi_err,
        }
    }
    return Result__File__GoError_Ok{
        _0: ffi_value_0,
    }
}

func close_file_ffi_wrap(p0 File) Result__unit__GoError {
    var ffi_err GoError = p0.Close()
    if ffi_err != nil {
        return Result__unit__GoError_Err{
            _0: ffi_err,
        }
    }
    return Result__unit__GoError_Ok{
        _0: struct{}{},
    }
}

func describe(path__0 string) string {
    var retv9 string
    var mtmp0 Result__File__GoError = open_file_ffi_wrap(path__0)
    var jp11 string
    switch mtmp0.(type) {
    case Result__File__GoError_Ok:
        var x1 File = mtmp0.(Result__File__GoError_Ok)._0
        var file__1 File = x1
        var name__2 string = file__1.Name()
        var mtmp3 Result__unit__GoError = close_file_ffi_wrap(file__1)
        var jp13 string
        switch mtmp3.(type) {
        case Result__unit__GoError_Ok:
            var jp15 string
            var t16 string = "ok=" + name__2
            jp15 = t16
            jp13 = jp15
            jp11 = jp13
            retv9 = jp11
            return retv9
        case Result__unit__GoError_Err:
            var x5 GoError = mtmp3.(Result__unit__GoError_Err)._0
            var err__3 GoError = x5
            var t17 string = go_error_to_string(err__3)
            var t18 string = "close_err=" + t17
            jp13 = t18
            jp11 = jp13
            retv9 = jp11
            return retv9
        default:
            panic("non-exhaustive match")
        }
    case Result__File__GoError_Err:
        var x2 GoError = mtmp0.(Result__File__GoError_Err)._0
        var err__4 GoError = x2
        var t19 string = go_error_to_string(err__4)
        var t20 string = "open_err=" + t19
        jp11 = t20
        retv9 = jp11
        return retv9
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t22 string = describe("/etc/hosts")
    println__T_string(t22)
    var t23 string = describe("/definitely/missing")
    println__T_string(t23)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
