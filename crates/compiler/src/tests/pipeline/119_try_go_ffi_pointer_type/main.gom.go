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

type Result__string__GoError interface {
    isResult__string__GoError()
}

type Result__string__GoError_Ok struct {
    _0 string
}

func (_ Result__string__GoError_Ok) isResult__string__GoError() {}

type Result__string__GoError_Err struct {
    _0 GoError
}

func (_ Result__string__GoError_Err) isResult__string__GoError() {}

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
    var ffi_value File
    var ffi_err GoError
    ffi_value, ffi_err = os.Open(p0)
    if ffi_err != nil {
        return Result__File__GoError_Err{
            _0: ffi_err,
        }
    }
    return Result__File__GoError_Ok{
        _0: ffi_value,
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

func describe(path__0 string) Result__string__GoError {
    var retv12 Result__string__GoError
    var mtmp0 Result__File__GoError = open_file_ffi_wrap(path__0)
    var jp14 File
    switch mtmp0.(type) {
    case Result__File__GoError_Ok:
        var x1 File = mtmp0.(Result__File__GoError_Ok)._0
        var try_value__4 File = x1
        jp14 = try_value__4
        var file__1 File = jp14
        var name__2 string = file__1.Name()
        var mtmp3 Result__unit__GoError = close_file_ffi_wrap(file__1)
        switch mtmp3.(type) {
        case Result__unit__GoError_Ok:
            var t16 string = "ok=" + name__2
            var t17 Result__string__GoError = Result__string__GoError_Ok{
                _0: t16,
            }
            retv12 = t17
            return retv12
        case Result__unit__GoError_Err:
            var x5 GoError = mtmp3.(Result__unit__GoError_Err)._0
            var try_residual__11 GoError = x5
            var t18 Result__string__GoError = Result__string__GoError_Err{
                _0: try_residual__11,
            }
            retv12 = t18
            return retv12
        default:
            panic("non-exhaustive match")
        }
    case Result__File__GoError_Err:
        var x2 GoError = mtmp0.(Result__File__GoError_Err)._0
        var try_residual__4 GoError = x2
        var t19 Result__string__GoError = Result__string__GoError_Err{
            _0: try_residual__4,
        }
        retv12 = t19
        return retv12
    default:
        panic("non-exhaustive match")
    }
}

func show(res__3 Result__string__GoError) string {
    var retv21 string
    var jp23 string
    switch res__3.(type) {
    case Result__string__GoError_Ok:
        var x7 string = res__3.(Result__string__GoError_Ok)._0
        var value__4 string = x7
        jp23 = value__4
    case Result__string__GoError_Err:
        var x8 GoError = res__3.(Result__string__GoError_Err)._0
        var err__5 GoError = x8
        var t24 string = go_error_to_string(err__5)
        var t25 string = "err=" + t24
        jp23 = t25
    default:
        panic("non-exhaustive match")
    }
    retv21 = jp23
    return retv21
}

func main0() struct{} {
    var t27 Result__string__GoError = describe("/etc/hosts")
    var t28 string = show(t27)
    println__T_string(t28)
    var t29 Result__string__GoError = describe("/definitely/missing")
    var t30 string = show(t29)
    println__T_string(t30)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
