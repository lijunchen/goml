package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string interface {
    is_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string()
}

type _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Ok struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Ok) is_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string() {}

type _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Err struct {
    _0 string
}

func (_ _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Err) is_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func split_host_port(ok__0 bool) _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string {
    var retv11 _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string
    var jp13 _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string
    if ok__0 {
        var t14 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var t15 _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string = _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Ok{
            _0: t14,
        }
        jp13 = t15
    } else {
        var t16 _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string = _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Err{
            _0: "missing port",
        }
        jp13 = t16
    }
    retv11 = jp13
    return retv11
}

func render(ok__1 bool) Result__string__string {
    var retv18 Result__string__string
    var mtmp0 _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string = split_host_port(ok__1)
    var jp20 Tuple2_6string_6string
    switch mtmp0.(type) {
    case _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Ok:
        var x1 Tuple2_6string_6string = mtmp0.(_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Ok)._0
        var try_value__14 Tuple2_6string_6string = x1
        jp20 = try_value__14
        var mtmp3 Tuple2_6string_6string = jp20
        var x4 string = mtmp3._0
        var x5 string = mtmp3._1
        var port__3 string = x5
        var host__2 string = x4
        var t21 string = host__2 + ":"
        var t22 string = t21 + port__3
        var t23 Result__string__string = Result__string__string_Ok{
            _0: t22,
        }
        retv18 = t23
        return retv18
    case _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Err:
        var x2 string = mtmp0.(_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Err)._0
        var try_residual__14 string = x2
        var t24 Result__string__string = Result__string__string_Err{
            _0: try_residual__14,
        }
        retv18 = t24
        return retv18
    default:
        panic("non-exhaustive match")
    }
}

func show(res__4 Result__string__string) string {
    var retv26 string
    var jp28 string
    switch res__4.(type) {
    case Result__string__string_Ok:
        var x6 string = res__4.(Result__string__string_Ok)._0
        var value__5 string = x6
        var t29 string = "ok " + value__5
        jp28 = t29
    case Result__string__string_Err:
        var x7 string = res__4.(Result__string__string_Err)._0
        var err__6 string = x7
        var t30 string = "err " + err__6
        jp28 = t30
    default:
        panic("non-exhaustive match")
    }
    retv26 = jp28
    return retv26
}

func main0() struct{} {
    var t32 Result__string__string = render(true)
    var t33 string = show(t32)
    println__T_string(t33)
    var t34 Result__string__string = render(false)
    var t35 string = show(t34)
    println__T_string(t35)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
