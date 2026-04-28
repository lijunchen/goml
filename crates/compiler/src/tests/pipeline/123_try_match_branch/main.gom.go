package main

import (
    _goml_fmt "fmt"
    _goml_pkg_time "time"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func go_error_to_string(value GoError) string {
    return value.Error()
}

type Mode int32

const (
    Left Mode = 0
    Keep Mode = 1
    Right Mode = 2
)

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

type Duration = _goml_pkg_time.Duration

func pick__native(mode__0 Mode, input__1 string) (string, GoError) {
    var jp14 string
    switch mode__0 {
    case Left:
        var jp17 Duration
        var mtmp0_value_0 Duration
        var mtmp0_err GoError
        mtmp0_value_0, mtmp0_err = _goml_pkg_time.ParseDuration(input__1)
        if mtmp0_err != nil {
            var ret_zero string
            return ret_zero, mtmp0_err
        }
        jp17 = mtmp0_value_0
        var parsed__2 Duration = jp17
        var t18 string = _goml_fmt.Sprintf("%v", parsed__2)
        var t19 string = "left=" + t18
        jp14 = t19
        var value__4 string = jp14
        return value__4, nil
    case Keep:
        jp14 = "keep"
        var value__4 string = jp14
        return value__4, nil
    case Right:
        var jp22 Duration
        var mtmp3_value_0 Duration
        var mtmp3_err GoError
        mtmp3_value_0, mtmp3_err = _goml_pkg_time.ParseDuration(input__1)
        if mtmp3_err != nil {
            var ret_zero string
            return ret_zero, mtmp3_err
        }
        jp22 = mtmp3_value_0
        var parsed__3 Duration = jp22
        var t23 string = _goml_fmt.Sprintf("%v", parsed__3)
        var t24 string = "right=" + t23
        jp14 = t24
        var value__4 string = jp14
        return value__4, nil
    default:
        panic("non-exhaustive match")
    }
}

func pick(mode__0 Mode, input__1 string) Result__string__GoError {
    var native_value_0 string
    var native_err GoError
    native_value_0, native_err = pick__native(mode__0, input__1)
    if native_err != nil {
        return Result__string__GoError_Err{
            _0: native_err,
        }
    }
    return Result__string__GoError_Ok{
        _0: native_value_0,
    }
}

func show(res__5 Result__string__GoError) string {
    var retv27 string
    var jp29 string
    switch res__5.(type) {
    case Result__string__GoError_Ok:
        var x6 string = res__5.(Result__string__GoError_Ok)._0
        var value__6 string = x6
        jp29 = value__6
    case Result__string__GoError_Err:
        var x7 GoError = res__5.(Result__string__GoError_Err)._0
        var err__7 GoError = x7
        var t30 string = go_error_to_string(err__7)
        var t31 string = "err=" + t30
        jp29 = t31
    default:
        panic("non-exhaustive match")
    }
    retv27 = jp29
    return retv27
}

func main0() struct{} {
    var t33 Result__string__GoError = pick(Left, "4s")
    var t34 string = show(t33)
    println__T_string(t34)
    var t35 Result__string__GoError = pick(Right, "bad")
    var t36 string = show(t35)
    println__T_string(t36)
    var t37 Result__string__GoError = pick(Keep, "ignored")
    var t38 string = show(t37)
    println__T_string(t38)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
