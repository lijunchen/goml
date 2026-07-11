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
    var retv18 _goml_m_Result_____o_string_c_string_q_____string
    var jp20 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t21 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t22 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t21,
        }
        jp20 = t22
    } else {
        var t23 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp20 = t23
    }
    retv18 = jp20
    return retv18
}

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv25 _goml_m_Result_____o_string_c_string_q_____string
    var t26 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv25 = t26
    return retv25
}

func render(ok__2 bool) Result__string__string {
    var retv28 Result__string__string
    var mtmp7 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp30 Tuple2_6string_6string
    switch mtmp7.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x8 Tuple2_6string_6string = mtmp7.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x8
        jp30 = try_value__17
        var mtmp10 Tuple2_6string_6string = jp30
        var x11 string = mtmp10._0
        var x12 string = mtmp10._1
        var port__4 string = x12
        var host__3 string = x11
        var t31 string = host__3 + "="
        var t32 string = t31 + port__4
        var t33 Result__string__string = Result__string__string_Ok{
            _0: t32,
        }
        retv28 = t33
        return retv28
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x9 string = mtmp7.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x9
        var t34 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv28 = t34
        return retv28
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv36 string
    var jp38 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x13 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x13
        var t39 string = "ok " + value__6
        jp38 = t39
    case Result__string__string_Err:
        var x14 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x14
        var t40 string = "err " + err__7
        jp38 = t40
    default:
        panic("non-exhaustive match")
    }
    retv36 = jp38
    return retv36
}

func main0() struct{} {
    var t42 Result__string__string = render(true)
    var t43 string = show(t42)
    println__T_string(t43)
    var t44 Result__string__string = render(false)
    var t45 string = show(t44)
    println__T_string(t45)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t47 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t47)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv50 string
    retv50 = self__9
    return retv50
}

func main() {
    main0()
}
