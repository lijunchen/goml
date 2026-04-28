package main

import (
    _goml_fmt "fmt"
    "time"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

func touch__native(text__0 string) (string, GoError) {
    var mtmp0_err GoError
    _, mtmp0_err = time.ParseDuration(text__0)
    if mtmp0_err != nil {
        var ret_zero string
        return ret_zero, mtmp0_err
    }
    return "ok", nil
}

func touch(text__0 string) Result__string__GoError {
    var native_value_0 string
    var native_err GoError
    native_value_0, native_err = touch__native(text__0)
    if native_err != nil {
        return Result__string__GoError_Err{
            _0: native_err,
        }
    }
    return Result__string__GoError_Ok{
        _0: native_value_0,
    }
}

func show(res__1 Result__string__GoError) string {
    var retv15 string
    var jp17 string
    switch res__1.(type) {
    case Result__string__GoError_Ok:
        var x4 string = res__1.(Result__string__GoError_Ok)._0
        var value__2 string = x4
        jp17 = value__2
    case Result__string__GoError_Err:
        var x5 GoError = res__1.(Result__string__GoError_Err)._0
        var err__3 GoError = x5
        var t18 string = go_error_to_string(err__3)
        var t19 string = "err=" + t18
        jp17 = t19
    default:
        panic("non-exhaustive match")
    }
    retv15 = jp17
    return retv15
}

func main0() struct{} {
    var t21 Result__string__GoError = touch("2s")
    var t22 string = show(t21)
    println__T_string(t22)
    var t23 Result__string__GoError = touch("bad")
    var t24 string = show(t23)
    println__T_string(t24)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
