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
            _0: "example.com",
            _1: "443",
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

func render(ok__1 bool) Result__string__string {
    var retv22 Result__string__string
    var mtmp4 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    var jp24 Tuple2_6string_6string
    switch mtmp4.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x5 Tuple2_6string_6string = mtmp4.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__14 Tuple2_6string_6string = x5
        jp24 = try_value__14
        var mtmp7 Tuple2_6string_6string = jp24
        var x8 string = mtmp7._0
        var x9 string = mtmp7._1
        var port__3 string = x9
        var host__2 string = x8
        var t25 string = host__2 + ":"
        var t26 string = t25 + port__3
        var t27 Result__string__string = Result__string__string_Ok{
            _0: t26,
        }
        retv22 = t27
        return retv22
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x6 string = mtmp4.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__14 string = x6
        var t28 Result__string__string = Result__string__string_Err{
            _0: try_residual__14,
        }
        retv22 = t28
        return retv22
    default:
        panic("non-exhaustive match")
    }
}

func show(res__4 Result__string__string) string {
    var retv30 string
    var jp32 string
    switch res__4.(type) {
    case Result__string__string_Ok:
        var x10 string = res__4.(Result__string__string_Ok)._0
        var value__5 string = x10
        var t33 string = "ok " + value__5
        jp32 = t33
    case Result__string__string_Err:
        var x11 string = res__4.(Result__string__string_Err)._0
        var err__6 string = x11
        var t34 string = "err " + err__6
        jp32 = t34
    default:
        panic("non-exhaustive match")
    }
    retv30 = jp32
    return retv30
}

func main0() struct{} {
    var t36 Result__string__string = render(true)
    var t37 string = show(t36)
    println__T_string(t37)
    var t38 Result__string__string = render(false)
    var t39 string = show(t38)
    println__T_string(t39)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t41 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t41)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv44 string
    retv44 = self__9
    return retv44
}

func main() {
    main0()
}
