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
            _0: "localhost",
            _1: "8080",
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

func pair(ok__1 bool) _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string {
    var retv18 _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string
    var t19 _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string = split_host_port(ok__1)
    retv18 = t19
    return retv18
}

func render(ok__2 bool) Result__string__string {
    var retv21 Result__string__string
    var mtmp0 _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string = pair(ok__2)
    var jp23 Tuple2_6string_6string
    switch mtmp0.(type) {
    case _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Ok:
        var x1 Tuple2_6string_6string = mtmp0.(_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x1
        jp23 = try_value__17
        var mtmp3 Tuple2_6string_6string = jp23
        var x4 string = mtmp3._0
        var x5 string = mtmp3._1
        var port__4 string = x5
        var host__3 string = x4
        var t24 string = host__3 + "="
        var t25 string = t24 + port__4
        var t26 Result__string__string = Result__string__string_Ok{
            _0: t25,
        }
        retv21 = t26
        return retv21
    case _goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Err:
        var x2 string = mtmp0.(_goml_Result_x5f__x5f__x28_string_x2c_string_x29__x5f__x5f_string_Err)._0
        var try_residual__17 string = x2
        var t27 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv21 = t27
        return retv21
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv29 string
    var jp31 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x6 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x6
        var t32 string = "ok " + value__6
        jp31 = t32
    case Result__string__string_Err:
        var x7 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x7
        var t33 string = "err " + err__7
        jp31 = t33
    default:
        panic("non-exhaustive match")
    }
    retv29 = jp31
    return retv29
}

func main0() struct{} {
    var t35 Result__string__string = render(true)
    var t36 string = show(t35)
    println__T_string(t36)
    var t37 Result__string__string = render(false)
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
