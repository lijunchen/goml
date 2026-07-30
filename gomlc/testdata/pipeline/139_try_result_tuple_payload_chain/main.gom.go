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
    var retv119 _goml_m_Result_____o_string_c_string_q_____string
    var jp121 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t122 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t123 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t122,
        }
        jp121 = t123
    } else {
        var t124 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp121 = t124
    }
    retv119 = jp121
    return retv119
}

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv126 _goml_m_Result_____o_string_c_string_q_____string
    var t127 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv126 = t127
    return retv126
}

func render(ok__2 bool) Result__string__string {
    var retv129 Result__string__string
    var mtmp108 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp131 Tuple2_6string_6string
    switch mtmp108.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x109 Tuple2_6string_6string = mtmp108.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x109
        jp131 = try_value__17
        var mtmp111 Tuple2_6string_6string = jp131
        var x112 string = mtmp111._0
        var x113 string = mtmp111._1
        var port__4 string = x113
        var host__3 string = x112
        var t132 string = host__3 + "="
        var t133 string = t132 + port__4
        var t134 Result__string__string = Result__string__string_Ok{
            _0: t133,
        }
        retv129 = t134
        return retv129
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x110 string = mtmp108.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x110
        var t135 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv129 = t135
        return retv129
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv137 string
    var jp139 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x114 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x114
        var t140 string = "ok " + value__6
        jp139 = t140
    case Result__string__string_Err:
        var x115 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x115
        var t141 string = "err " + err__7
        jp139 = t141
    default:
        panic("non-exhaustive match")
    }
    retv137 = jp139
    return retv137
}

func main0() struct{} {
    var t143 Result__string__string = render(true)
    var t144 string = show(t143)
    println__T_string(t144)
    var t145 Result__string__string = render(false)
    var t146 string = show(t145)
    println__T_string(t146)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv151 string
    retv151 = self__38
    return retv151
}

func main() {
    main0()
}
