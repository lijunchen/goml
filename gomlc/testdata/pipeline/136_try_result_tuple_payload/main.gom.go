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
            _0: "example.com",
            _1: "443",
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

func render(ok__1 bool) Result__string__string {
    var retv86 Result__string__string
    var mtmp68 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    var jp88 Tuple2_6string_6string
    switch mtmp68.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x69 Tuple2_6string_6string = mtmp68.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__14 Tuple2_6string_6string = x69
        jp88 = try_value__14
        var mtmp71 Tuple2_6string_6string = jp88
        var x72 string = mtmp71._0
        var x73 string = mtmp71._1
        var port__3 string = x73
        var host__2 string = x72
        var t89 string = host__2 + ":"
        var t90 string = t89 + port__3
        var t91 Result__string__string = Result__string__string_Ok{
            _0: t90,
        }
        retv86 = t91
        return retv86
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x70 string = mtmp68.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__14 string = x70
        var t92 Result__string__string = Result__string__string_Err{
            _0: try_residual__14,
        }
        retv86 = t92
        return retv86
    default:
        panic("non-exhaustive match")
    }
}

func show(res__4 Result__string__string) string {
    var retv94 string
    var jp96 string
    switch res__4.(type) {
    case Result__string__string_Ok:
        var x74 string = res__4.(Result__string__string_Ok)._0
        var value__5 string = x74
        var t97 string = "ok " + value__5
        jp96 = t97
    case Result__string__string_Err:
        var x75 string = res__4.(Result__string__string_Err)._0
        var err__6 string = x75
        var t98 string = "err " + err__6
        jp96 = t98
    default:
        panic("non-exhaustive match")
    }
    retv94 = jp96
    return retv94
}

func main0() struct{} {
    var t100 Result__string__string = render(true)
    var t101 string = show(t100)
    println__T_string(t101)
    var t102 Result__string__string = render(false)
    var t103 string = show(t102)
    println__T_string(t103)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t105)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv108 string
    retv108 = self__38
    return retv108
}

func main() {
    main0()
}
