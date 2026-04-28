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

type GoError = error

type Duration = _goml_pkg_time.Duration

func parse_text__native(input__0 string) (Duration, GoError) {
    var t12_value_0 Duration
    var t12_err GoError
    t12_value_0, t12_err = _goml_pkg_time.ParseDuration(input__0)
    if t12_err != nil {
        var ret_zero Duration
        return ret_zero, t12_err
    }
    return t12_value_0, nil
}

func normalize__native(input__1 string) (Duration, GoError) {
    var jp16 Duration
    var mtmp0_value_0 Duration
    var mtmp0_err GoError
    mtmp0_value_0, mtmp0_err = parse_text__native(input__1)
    if mtmp0_err != nil {
        var ret_zero Duration
        return ret_zero, mtmp0_err
    }
    jp16 = mtmp0_value_0
    var value__2 Duration = jp16
    return value__2, nil
}

func decorate__native(input__3 string) (string, GoError) {
    var jp22 Duration
    var mtmp3_value_0 Duration
    var mtmp3_err GoError
    mtmp3_value_0, mtmp3_err = normalize__native(input__3)
    if mtmp3_err != nil {
        var ret_zero string
        return ret_zero, mtmp3_err
    }
    jp22 = mtmp3_value_0
    var value__4 Duration = jp22
    var t23 string = _goml_fmt.Sprintf("%v", value__4)
    var t24 string = "dur=" + t23
    return t24, nil
}

func decorate(input__3 string) Result__string__GoError {
    var native_value_0 string
    var native_err GoError
    native_value_0, native_err = decorate__native(input__3)
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
    var retv28 string
    var jp30 string
    switch res__5.(type) {
    case Result__string__GoError_Ok:
        var x6 string = res__5.(Result__string__GoError_Ok)._0
        var value__6 string = x6
        jp30 = value__6
    case Result__string__GoError_Err:
        var x7 GoError = res__5.(Result__string__GoError_Err)._0
        var err__7 GoError = x7
        var t31 string = go_error_to_string(err__7)
        var t32 string = "err=" + t31
        jp30 = t32
    default:
        panic("non-exhaustive match")
    }
    retv28 = jp30
    return retv28
}

func main0() struct{} {
    var t34 Result__string__GoError = decorate("3s")
    var t35 string = show(t34)
    println__T_string(t35)
    var t36 Result__string__GoError = decorate("oops")
    var t37 string = show(t36)
    println__T_string(t37)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
