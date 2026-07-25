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
    var retv75 _goml_m_Result_____o_string_c_string_q_____string
    var jp77 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t78 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t79 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t78,
        }
        jp77 = t79
    } else {
        var t80 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp77 = t80
    }
    retv75 = jp77
    return retv75
}

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv82 _goml_m_Result_____o_string_c_string_q_____string
    var t83 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv82 = t83
    return retv82
}

func render(ok__2 bool) Result__string__string {
    var retv85 Result__string__string
    var mtmp64 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp87 Tuple2_6string_6string
    switch mtmp64.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x65 Tuple2_6string_6string = mtmp64.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x65
        jp87 = try_value__17
        var mtmp67 Tuple2_6string_6string = jp87
        var x68 string = mtmp67._0
        var x69 string = mtmp67._1
        var port__4 string = x69
        var host__3 string = x68
        var t88 string = host__3 + "="
        var t89 string = t88 + port__4
        var t90 Result__string__string = Result__string__string_Ok{
            _0: t89,
        }
        retv85 = t90
        return retv85
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x66 string = mtmp64.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x66
        var t91 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv85 = t91
        return retv85
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv93 string
    var jp95 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x70 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x70
        var t96 string = "ok " + value__6
        jp95 = t96
    case Result__string__string_Err:
        var x71 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x71
        var t97 string = "err " + err__7
        jp95 = t97
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var t99 Result__string__string = render(true)
    var t100 string = show(t99)
    println__T_string(t100)
    var t101 Result__string__string = render(false)
    var t102 string = show(t101)
    println__T_string(t102)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv107 string
    retv107 = self__38
    return retv107
}

func main() {
    main0()
}
