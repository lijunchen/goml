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
    var retv72 _goml_m_Result_____o_string_c_string_q_____string
    var jp74 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t75 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var t76 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t75,
        }
        jp74 = t76
    } else {
        var t77 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp74 = t77
    }
    retv72 = jp74
    return retv72
}

func render(ok__1 bool) Result__string__string {
    var retv79 Result__string__string
    var mtmp61 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    var jp81 Tuple2_6string_6string
    switch mtmp61.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x62 Tuple2_6string_6string = mtmp61.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__14 Tuple2_6string_6string = x62
        jp81 = try_value__14
        var mtmp64 Tuple2_6string_6string = jp81
        var x65 string = mtmp64._0
        var x66 string = mtmp64._1
        var port__3 string = x66
        var host__2 string = x65
        var t82 string = host__2 + ":"
        var t83 string = t82 + port__3
        var t84 Result__string__string = Result__string__string_Ok{
            _0: t83,
        }
        retv79 = t84
        return retv79
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x63 string = mtmp61.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__14 string = x63
        var t85 Result__string__string = Result__string__string_Err{
            _0: try_residual__14,
        }
        retv79 = t85
        return retv79
    default:
        panic("non-exhaustive match")
    }
}

func show(res__4 Result__string__string) string {
    var retv87 string
    var jp89 string
    switch res__4.(type) {
    case Result__string__string_Ok:
        var x67 string = res__4.(Result__string__string_Ok)._0
        var value__5 string = x67
        var t90 string = "ok " + value__5
        jp89 = t90
    case Result__string__string_Err:
        var x68 string = res__4.(Result__string__string_Err)._0
        var err__6 string = x68
        var t91 string = "err " + err__6
        jp89 = t91
    default:
        panic("non-exhaustive match")
    }
    retv87 = jp89
    return retv87
}

func main0() struct{} {
    var t93 Result__string__string = render(true)
    var t94 string = show(t93)
    println__T_string(t94)
    var t95 Result__string__string = render(false)
    var t96 string = show(t95)
    println__T_string(t96)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv101 string
    retv101 = self__37
    return retv101
}

func main() {
    main0()
}
