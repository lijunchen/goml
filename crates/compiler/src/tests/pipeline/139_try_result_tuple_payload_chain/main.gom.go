package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type _goml_m_Result_____o_string_c_string_q_____string interface {
    is_goml_m_Result_____o_string_c_string_q_____string()
}

type _goml_m_Result_____o_string_c_string_q_____string_Ok struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Result_____o_string_c_string_q_____string_Ok) is_goml_m_Result_____o_string_c_string_q_____string() {}

type _goml_m_Result_____o_string_c_string_q_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_string_c_string_q_____string_Err) is_goml_m_Result_____o_string_c_string_q_____string() {}

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

func split_host_port(ok__0 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv15 _goml_m_Result_____o_string_c_string_q_____string
    var jp17 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t18 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t19 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t18,
        }
        jp17 = t19
    } else {
        var t20 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp17 = t20
    }
    retv15 = jp17
    return retv15
}

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv22 _goml_m_Result_____o_string_c_string_q_____string
    var t23 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv22 = t23
    return retv22
}

func render(ok__2 bool) Result__string__string {
    var retv25 Result__string__string
    var mtmp4 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp27 Tuple2_6string_6string
    switch mtmp4.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x5 Tuple2_6string_6string = mtmp4.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x5
        jp27 = try_value__17
        var mtmp7 Tuple2_6string_6string = jp27
        var x8 string = mtmp7._0
        var x9 string = mtmp7._1
        var port__4 string = x9
        var host__3 string = x8
        var t28 string = host__3 + "="
        var t29 string = t28 + port__4
        var t30 Result__string__string = Result__string__string_Ok{
            _0: t29,
        }
        retv25 = t30
        return retv25
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x6 string = mtmp4.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x6
        var t31 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv25 = t31
        return retv25
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv33 string
    var jp35 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x10 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x10
        var t36 string = "ok " + value__6
        jp35 = t36
    case Result__string__string_Err:
        var x11 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x11
        var t37 string = "err " + err__7
        jp35 = t37
    default:
        panic("non-exhaustive match")
    }
    retv33 = jp35
    return retv33
}

func main0() struct{} {
    var t39 Result__string__string = render(true)
    var t40 string = show(t39)
    println__T_string(t40)
    var t41 Result__string__string = render(false)
    var t42 string = show(t41)
    println__T_string(t42)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t44 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t44)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv47 string
    retv47 = self__9
    return retv47
}

func main() {
    main0()
}
