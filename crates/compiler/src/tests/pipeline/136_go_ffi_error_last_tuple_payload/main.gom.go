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

type _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_GoError interface {
    is_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_GoError()
}

type _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_GoError_Ok struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_GoError_Ok) is_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_GoError() {}

type _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_GoError_Err struct {
    _0 GoError
}

func (_ _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_GoError_Err) is_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_GoError() {}

type GoError = error

func render__native(text__0 string) (string, GoError) {
    var jp12 Tuple2_6string_6string
    var mtmp0_value_0 string
    var mtmp0_value_1 string
    var mtmp0_err GoError
    mtmp0_value_0, mtmp0_value_1, mtmp0_err = net.SplitHostPort(text__0)
    if mtmp0_err != nil {
        var ret_zero string
        return ret_zero, mtmp0_err
    }
    jp12 = Tuple2_6string_6string{
        _0: mtmp0_value_0,
        _1: mtmp0_value_1,
    }
    var mtmp3 Tuple2_6string_6string = jp12
    var x4 string = mtmp3._0
    var x5 string = mtmp3._1
    var port__2 string = x5
    var host__1 string = x4
    var t13 string = host__1 + "|"
    var t14 string = t13 + port__2
    return t14, nil
}

func render(text__0 string) Result__string__GoError {
    var native_value_0 string
    var native_err GoError
    native_value_0, native_err = render__native(text__0)
    if native_err != nil {
        return Result__string__GoError_Err{
            _0: native_err,
        }
    }
    return Result__string__GoError_Ok{
        _0: native_value_0,
    }
}

func show(res__3 Result__string__GoError) string {
    var retv18 string
    var jp20 string
    switch res__3.(type) {
    case Result__string__GoError_Ok:
        var x6 string = res__3.(Result__string__GoError_Ok)._0
        var text__4 string = x6
        jp20 = text__4
    case Result__string__GoError_Err:
        var x7 GoError = res__3.(Result__string__GoError_Err)._0
        var err__5 GoError = x7
        var t21 string = go_error_to_string(err__5)
        jp20 = t21
    default:
        panic("non-exhaustive match")
    }
    retv18 = jp20
    return retv18
}

func main0() struct{} {
    var t23 Result__string__GoError = render("example.com:443")
    var t24 string = show(t23)
    string_println(t24)
    return struct{}{}
}

func main() {
    main0()
}
