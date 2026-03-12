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

func configure_and_format__native(input__0 string) (string, GoError) {
    var mtmp0_err GoError = os.Setenv("GOML_TRY_FFI", "ok")
    if mtmp0_err != nil {
        var ret_zero string
        return ret_zero, mtmp0_err
    }
    var jp15 Duration
    var mtmp4_value Duration
    var mtmp4_err GoError
    mtmp4_value, mtmp4_err = time.ParseDuration(input__0)
    if mtmp4_err != nil {
        var ret_zero string
        return ret_zero, mtmp4_err
    }
    jp15 = mtmp4_value
    var value__1 Duration = jp15
    var t16 string = fmt.Sprintf("dur=%v", value__1)
    return t16, nil
}

func configure_and_format(input__0 string) Result__string__GoError {
    var native_value string
    var native_err GoError
    native_value, native_err = configure_and_format__native(input__0)
    if native_err != nil {
        return Result__string__GoError_Err{
            _0: native_err,
        }
    }
    return Result__string__GoError_Ok{
        _0: native_value,
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
