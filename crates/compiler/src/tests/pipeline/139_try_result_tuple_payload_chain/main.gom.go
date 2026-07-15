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
    var retv33 _goml_m_Result_____o_string_c_string_q_____string
    var jp35 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t36 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t37 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t36,
        }
        jp35 = t37
    } else {
        var t38 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp35 = t38
    }
    retv33 = jp35
    return retv33
}

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv40 _goml_m_Result_____o_string_c_string_q_____string
    var t41 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv40 = t41
    return retv40
}

func render(ok__2 bool) Result__string__string {
    var retv43 Result__string__string
    var mtmp22 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp45 Tuple2_6string_6string
    switch mtmp22.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x23 Tuple2_6string_6string = mtmp22.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x23
        jp45 = try_value__17
        var mtmp25 Tuple2_6string_6string = jp45
        var x26 string = mtmp25._0
        var x27 string = mtmp25._1
        var port__4 string = x27
        var host__3 string = x26
        var t46 string = host__3 + "="
        var t47 string = t46 + port__4
        var t48 Result__string__string = Result__string__string_Ok{
            _0: t47,
        }
        retv43 = t48
        return retv43
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x24 string = mtmp22.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x24
        var t49 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv43 = t49
        return retv43
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv51 string
    var jp53 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x28 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x28
        var t54 string = "ok " + value__6
        jp53 = t54
    case Result__string__string_Err:
        var x29 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x29
        var t55 string = "err " + err__7
        jp53 = t55
    default:
        panic("non-exhaustive match")
    }
    retv51 = jp53
    return retv51
}

func main0() struct{} {
    var t57 Result__string__string = render(true)
    var t58 string = show(t57)
    println__T_string(t58)
    var t59 Result__string__string = render(false)
    var t60 string = show(t59)
    println__T_string(t60)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t62 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t62)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv65 string
    retv65 = self__9
    return retv65
}

func main() {
    main0()
}
