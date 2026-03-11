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

func describe_duration(input__0 string) string {
    var retv10 string
    var mtmp0 Result__Duration__GoError = parse_duration_ffi_wrap(input__0)
    var jp12 string
    switch mtmp0.(type) {
    case Result__Duration__GoError_Ok:
        var x1 Duration = mtmp0.(Result__Duration__GoError_Ok)._0
        var value__1 Duration = x1
        var t13 string = fmt.Sprintf("ok=%v", value__1)
        jp12 = t13
    case Result__Duration__GoError_Err:
        var x2 GoError = mtmp0.(Result__Duration__GoError_Err)._0
        var err__2 GoError = x2
        var t14 string = go_error_to_string(err__2)
        var t15 string = "err=" + t14
        jp12 = t15
    default:
        panic("non-exhaustive match")
    }
    retv10 = jp12
    return retv10
}

func describe_setenv() string {
    var retv17 string
    var mtmp3 Result__unit__GoError = setenv_ffi_wrap("GOML_FFI_TEST", "ok")
    var jp19 string
    switch mtmp3.(type) {
    case Result__unit__GoError_Ok:
        var jp21 string
        jp21 = "setenv=ok"
        jp19 = jp21
        retv17 = jp19
        return retv17
    case Result__unit__GoError_Err:
        var x5 GoError = mtmp3.(Result__unit__GoError_Err)._0
        var err__3 GoError = x5
        var t22 string = go_error_to_string(err__3)
        var t23 string = "setenv_err=" + t22
        jp19 = t23
        retv17 = jp19
        return retv17
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t25 string = describe_duration("1.5s")
    println__T_string(t25)
    var t26 string = describe_duration("bad")
    println__T_string(t26)
    var t27 string = describe_setenv()
    println__T_string(t27)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
