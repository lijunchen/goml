package main

import (
    _goml_fmt "fmt"
    "net"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func go_error_to_string(value GoError) string {
    return value.Error()
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type _goml_Result___x28_string_x2c_string_x29___GoError interface {
    is_goml_Result___x28_string_x2c_string_x29___GoError()
}

type _goml_Result___x28_string_x2c_string_x29___GoError_Ok struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_Result___x28_string_x2c_string_x29___GoError_Ok) is_goml_Result___x28_string_x2c_string_x29___GoError() {}

type _goml_Result___x28_string_x2c_string_x29___GoError_Err struct {
    _0 GoError
}

func (_ _goml_Result___x28_string_x2c_string_x29___GoError_Err) is_goml_Result___x28_string_x2c_string_x29___GoError() {}

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

func pair__native(text__0 string) (string, string, GoError) {
    var t11_value_0 string
    var t11_value_1 string
    var t11_err GoError
    t11_value_0, t11_value_1, t11_err = net.SplitHostPort(text__0)
    if t11_err != nil {
        var ret_zero Tuple2_6string_6string
        return ret_zero._0, ret_zero._1, t11_err
    }
    var ret_payload Tuple2_6string_6string = Tuple2_6string_6string{
        _0: t11_value_0,
        _1: t11_value_1,
    }
    return ret_payload._0, ret_payload._1, nil
}

func render__native(text__1 string) (string, GoError) {
    var jp15 Tuple2_6string_6string
    var mtmp0_value_0 string
    var mtmp0_value_1 string
    var mtmp0_err GoError
    mtmp0_value_0, mtmp0_value_1, mtmp0_err = pair__native(text__1)
    if mtmp0_err != nil {
        var ret_zero string
        return ret_zero, mtmp0_err
    }
    jp15 = Tuple2_6string_6string{
        _0: mtmp0_value_0,
        _1: mtmp0_value_1,
    }
    var mtmp3 Tuple2_6string_6string = jp15
    var x4 string = mtmp3._0
    var x5 string = mtmp3._1
    var port__3 string = x5
    var host__2 string = x4
    var t16 string = host__2 + "|"
    var t17 string = t16 + port__3
    return t17, nil
}

func render(text__1 string) Result__string__GoError {
    var native_value_0 string
    var native_err GoError
    native_value_0, native_err = render__native(text__1)
    if native_err != nil {
        return Result__string__GoError_Err{
            _0: native_err,
        }
    }
    return Result__string__GoError_Ok{
        _0: native_value_0,
    }
}

func show(res__4 Result__string__GoError) string {
    var retv21 string
    var jp23 string
    switch res__4.(type) {
    case Result__string__GoError_Ok:
        var x6 string = res__4.(Result__string__GoError_Ok)._0
        var text__5 string = x6
        jp23 = text__5
    case Result__string__GoError_Err:
        var x7 GoError = res__4.(Result__string__GoError_Err)._0
        var err__6 GoError = x7
        var t24 string = go_error_to_string(err__6)
        var t25 string = "err=" + t24
        jp23 = t25
    default:
        panic("non-exhaustive match")
    }
    retv21 = jp23
    return retv21
}

func main0() struct{} {
    var t27 Result__string__GoError = render("example.com:443")
    var t28 string = show(t27)
    println__T_string(t28)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
