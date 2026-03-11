package main

import (
    "fmt"
    "time"
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

type Result__Duration__GoError interface {
    isResult__Duration__GoError()
}

type Result__Duration__GoError_Ok struct {
    _0 Duration
}

func (_ Result__Duration__GoError_Ok) isResult__Duration__GoError() {}

type Result__Duration__GoError_Err struct {
    _0 GoError
}

func (_ Result__Duration__GoError_Err) isResult__Duration__GoError() {}

type GoError = error

type Duration = time.Duration

func parse_duration_ffi_wrap(p0 string) Result__Duration__GoError {
    var ffi_value Duration
    var ffi_err GoError
    ffi_value, ffi_err = time.ParseDuration(p0)
    if ffi_err != nil {
        return Result__Duration__GoError_Err{
            _0: ffi_err,
        }
    }
    return Result__Duration__GoError_Ok{
        _0: ffi_value,
    }
}

func setenv_ffi_wrap(p0 string, p1 string) Result__unit__GoError {
    var ffi_err GoError = os.Setenv(p0, p1)
    if ffi_err != nil {
        return Result__unit__GoError_Err{
            _0: ffi_err,
        }
    }
    return Result__unit__GoError_Ok{
        _0: struct{}{},
    }
}

func configure_and_format(input__0 string) Result__string__GoError {
    var retv12 Result__string__GoError
    var mtmp0 Result__unit__GoError = setenv_ffi_wrap("GOML_TRY_FFI", "ok")
    switch mtmp0.(type) {
    case Result__unit__GoError_Ok:
        var mtmp4 Result__Duration__GoError = parse_duration_ffi_wrap(input__0)
        var jp15 Duration
        switch mtmp4.(type) {
        case Result__Duration__GoError_Ok:
            var x5 Duration = mtmp4.(Result__Duration__GoError_Ok)._0
            var try_value__9 Duration = x5
            jp15 = try_value__9
            var value__1 Duration = jp15
            var t16 string = fmt.Sprintf("dur=%v", value__1)
            var t17 Result__string__GoError = Result__string__GoError_Ok{
                _0: t16,
            }
            retv12 = t17
            return retv12
        case Result__Duration__GoError_Err:
            var x6 GoError = mtmp4.(Result__Duration__GoError_Err)._0
            var try_residual__9 GoError = x6
            var t18 Result__string__GoError = Result__string__GoError_Err{
                _0: try_residual__9,
            }
            retv12 = t18
            return retv12
        default:
            panic("non-exhaustive match")
        }
    case Result__unit__GoError_Err:
        var x2 GoError = mtmp0.(Result__unit__GoError_Err)._0
        var try_residual__5 GoError = x2
        var t19 Result__string__GoError = Result__string__GoError_Err{
            _0: try_residual__5,
        }
        retv12 = t19
        return retv12
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__GoError) string {
    var retv21 string
    var jp23 string
    switch res__2.(type) {
    case Result__string__GoError_Ok:
        var x7 string = res__2.(Result__string__GoError_Ok)._0
        var value__3 string = x7
        jp23 = value__3
    case Result__string__GoError_Err:
        var x8 GoError = res__2.(Result__string__GoError_Err)._0
        var err__4 GoError = x8
        var t24 string = go_error_to_string(err__4)
        var t25 string = "err=" + t24
        jp23 = t25
    default:
        panic("non-exhaustive match")
    }
    retv21 = jp23
    return retv21
}

func main0() struct{} {
    var t27 Result__string__GoError = configure_and_format("2s")
    var t28 string = show(t27)
    println__T_string(t28)
    var t29 Result__string__GoError = configure_and_format("bad")
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
