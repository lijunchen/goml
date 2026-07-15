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
            _0: "example.com",
            _1: "443",
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

func render(ok__1 bool) Result__string__string {
    var retv40 Result__string__string
    var mtmp22 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    var jp42 Tuple2_6string_6string
    switch mtmp22.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x23 Tuple2_6string_6string = mtmp22.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__14 Tuple2_6string_6string = x23
        jp42 = try_value__14
        var mtmp25 Tuple2_6string_6string = jp42
        var x26 string = mtmp25._0
        var x27 string = mtmp25._1
        var port__3 string = x27
        var host__2 string = x26
        var t43 string = host__2 + ":"
        var t44 string = t43 + port__3
        var t45 Result__string__string = Result__string__string_Ok{
            _0: t44,
        }
        retv40 = t45
        return retv40
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x24 string = mtmp22.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__14 string = x24
        var t46 Result__string__string = Result__string__string_Err{
            _0: try_residual__14,
        }
        retv40 = t46
        return retv40
    default:
        panic("non-exhaustive match")
    }
}

func show(res__4 Result__string__string) string {
    var retv48 string
    var jp50 string
    switch res__4.(type) {
    case Result__string__string_Ok:
        var x28 string = res__4.(Result__string__string_Ok)._0
        var value__5 string = x28
        var t51 string = "ok " + value__5
        jp50 = t51
    case Result__string__string_Err:
        var x29 string = res__4.(Result__string__string_Err)._0
        var err__6 string = x29
        var t52 string = "err " + err__6
        jp50 = t52
    default:
        panic("non-exhaustive match")
    }
    retv48 = jp50
    return retv48
}

func main0() struct{} {
    var t54 Result__string__string = render(true)
    var t55 string = show(t54)
    println__T_string(t55)
    var t56 Result__string__string = render(false)
    var t57 string = show(t56)
    println__T_string(t57)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t59 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t59)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv62 string
    retv62 = self__9
    return retv62
}

func main() {
    main0()
}
