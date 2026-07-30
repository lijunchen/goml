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
    var retv79 _goml_m_Result_____o_string_c_string_q_____string
    var jp81 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t82 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t83 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t82,
        }
        jp81 = t83
    } else {
        var t84 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp81 = t84
    }
    retv79 = jp81
    return retv79
}

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv86 _goml_m_Result_____o_string_c_string_q_____string
    var t87 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv86 = t87
    return retv86
}

func render(ok__2 bool) Result__string__string {
    var retv89 Result__string__string
    var mtmp68 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp91 Tuple2_6string_6string
    switch mtmp68.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x69 Tuple2_6string_6string = mtmp68.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x69
        jp91 = try_value__17
        var mtmp71 Tuple2_6string_6string = jp91
        var x72 string = mtmp71._0
        var x73 string = mtmp71._1
        var port__4 string = x73
        var host__3 string = x72
        var t92 string = host__3 + "="
        var t93 string = t92 + port__4
        var t94 Result__string__string = Result__string__string_Ok{
            _0: t93,
        }
        retv89 = t94
        return retv89
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x70 string = mtmp68.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x70
        var t95 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv89 = t95
        return retv89
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv97 string
    var jp99 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x74 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x74
        var t100 string = "ok " + value__6
        jp99 = t100
    case Result__string__string_Err:
        var x75 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x75
        var t101 string = "err " + err__7
        jp99 = t101
    default:
        panic("non-exhaustive match")
    }
    retv97 = jp99
    return retv97
}

func main0() struct{} {
    var t103 Result__string__string = render(true)
    var t104 string = show(t103)
    println__T_string(t104)
    var t105 Result__string__string = render(false)
    var t106 string = show(t105)
    println__T_string(t106)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv111 string
    retv111 = self__38
    return retv111
}

func main() {
    main0()
}
