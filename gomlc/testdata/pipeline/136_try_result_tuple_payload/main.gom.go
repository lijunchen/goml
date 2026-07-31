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
    var retv163 _goml_m_Result_____o_string_c_string_q_____string
    var jp165 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t166 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var t167 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t166,
        }
        jp165 = t167
    } else {
        var t168 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp165 = t168
    }
    retv163 = jp165
    return retv163
}

func render(ok__1 bool) Result__string__string {
    var retv170 Result__string__string
    var mtmp152 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    var jp172 Tuple2_6string_6string
    switch mtmp152.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x153 Tuple2_6string_6string = mtmp152.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__14 Tuple2_6string_6string = x153
        jp172 = try_value__14
        var mtmp155 Tuple2_6string_6string = jp172
        var x156 string = mtmp155._0
        var x157 string = mtmp155._1
        var port__3 string = x157
        var host__2 string = x156
        var t173 string = host__2 + ":"
        var t174 string = t173 + port__3
        var t175 Result__string__string = Result__string__string_Ok{
            _0: t174,
        }
        retv170 = t175
        return retv170
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x154 string = mtmp152.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__14 string = x154
        var t176 Result__string__string = Result__string__string_Err{
            _0: try_residual__14,
        }
        retv170 = t176
        return retv170
    default:
        panic("non-exhaustive match")
    }
}

func show(res__4 Result__string__string) string {
    var retv178 string
    var jp180 string
    switch res__4.(type) {
    case Result__string__string_Ok:
        var x158 string = res__4.(Result__string__string_Ok)._0
        var value__5 string = x158
        var t181 string = "ok " + value__5
        jp180 = t181
    case Result__string__string_Err:
        var x159 string = res__4.(Result__string__string_Err)._0
        var err__6 string = x159
        var t182 string = "err " + err__6
        jp180 = t182
    default:
        panic("non-exhaustive match")
    }
    retv178 = jp180
    return retv178
}

func main0() struct{} {
    var t184 Result__string__string = render(true)
    var t185 string = show(t184)
    println__T_string(t185)
    var t186 Result__string__string = render(false)
    var t187 string = show(t186)
    println__T_string(t187)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv192 string
    retv192 = self__38
    return retv192
}

func main() {
    main0()
}
