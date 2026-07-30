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
            _0: "example.com",
            _1: "443",
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

func render(ok__1 bool) Result__string__string {
    var retv126 Result__string__string
    var mtmp108 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    var jp128 Tuple2_6string_6string
    switch mtmp108.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x109 Tuple2_6string_6string = mtmp108.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__14 Tuple2_6string_6string = x109
        jp128 = try_value__14
        var mtmp111 Tuple2_6string_6string = jp128
        var x112 string = mtmp111._0
        var x113 string = mtmp111._1
        var port__3 string = x113
        var host__2 string = x112
        var t129 string = host__2 + ":"
        var t130 string = t129 + port__3
        var t131 Result__string__string = Result__string__string_Ok{
            _0: t130,
        }
        retv126 = t131
        return retv126
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x110 string = mtmp108.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__14 string = x110
        var t132 Result__string__string = Result__string__string_Err{
            _0: try_residual__14,
        }
        retv126 = t132
        return retv126
    default:
        panic("non-exhaustive match")
    }
}

func show(res__4 Result__string__string) string {
    var retv134 string
    var jp136 string
    switch res__4.(type) {
    case Result__string__string_Ok:
        var x114 string = res__4.(Result__string__string_Ok)._0
        var value__5 string = x114
        var t137 string = "ok " + value__5
        jp136 = t137
    case Result__string__string_Err:
        var x115 string = res__4.(Result__string__string_Err)._0
        var err__6 string = x115
        var t138 string = "err " + err__6
        jp136 = t138
    default:
        panic("non-exhaustive match")
    }
    retv134 = jp136
    return retv134
}

func main0() struct{} {
    var t140 Result__string__string = render(true)
    var t141 string = show(t140)
    println__T_string(t141)
    var t142 Result__string__string = render(false)
    var t143 string = show(t142)
    println__T_string(t143)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t145 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t145)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv148 string
    retv148 = self__38
    return retv148
}

func main() {
    main0()
}
