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
            _0: "localhost",
            _1: "8080",
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

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv79 _goml_m_Result_____o_string_c_string_q_____string
    var t80 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv79 = t80
    return retv79
}

func render(ok__2 bool) Result__string__string {
    var retv82 Result__string__string
    var mtmp61 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp84 Tuple2_6string_6string
    switch mtmp61.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x62 Tuple2_6string_6string = mtmp61.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x62
        jp84 = try_value__17
        var mtmp64 Tuple2_6string_6string = jp84
        var x65 string = mtmp64._0
        var x66 string = mtmp64._1
        var port__4 string = x66
        var host__3 string = x65
        var t85 string = host__3 + "="
        var t86 string = t85 + port__4
        var t87 Result__string__string = Result__string__string_Ok{
            _0: t86,
        }
        retv82 = t87
        return retv82
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x63 string = mtmp61.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x63
        var t88 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv82 = t88
        return retv82
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv90 string
    var jp92 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x67 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x67
        var t93 string = "ok " + value__6
        jp92 = t93
    case Result__string__string_Err:
        var x68 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x68
        var t94 string = "err " + err__7
        jp92 = t94
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func main0() struct{} {
    var t96 Result__string__string = render(true)
    var t97 string = show(t96)
    println__T_string(t97)
    var t98 Result__string__string = render(false)
    var t99 string = show(t98)
    println__T_string(t99)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv104 string
    retv104 = self__37
    return retv104
}

func main() {
    main0()
}
