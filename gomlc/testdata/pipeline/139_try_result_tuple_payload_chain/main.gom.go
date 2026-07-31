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
            _0: "localhost",
            _1: "8080",
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

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv170 _goml_m_Result_____o_string_c_string_q_____string
    var t171 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv170 = t171
    return retv170
}

func render(ok__2 bool) Result__string__string {
    var retv173 Result__string__string
    var mtmp152 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp175 Tuple2_6string_6string
    switch mtmp152.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x153 Tuple2_6string_6string = mtmp152.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x153
        jp175 = try_value__17
        var mtmp155 Tuple2_6string_6string = jp175
        var x156 string = mtmp155._0
        var x157 string = mtmp155._1
        var port__4 string = x157
        var host__3 string = x156
        var t176 string = host__3 + "="
        var t177 string = t176 + port__4
        var t178 Result__string__string = Result__string__string_Ok{
            _0: t177,
        }
        retv173 = t178
        return retv173
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x154 string = mtmp152.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x154
        var t179 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv173 = t179
        return retv173
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv181 string
    var jp183 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x158 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x158
        var t184 string = "ok " + value__6
        jp183 = t184
    case Result__string__string_Err:
        var x159 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x159
        var t185 string = "err " + err__7
        jp183 = t185
    default:
        panic("non-exhaustive match")
    }
    retv181 = jp183
    return retv181
}

func main0() struct{} {
    var t187 Result__string__string = render(true)
    var t188 string = show(t187)
    println__T_string(t188)
    var t189 Result__string__string = render(false)
    var t190 string = show(t189)
    println__T_string(t190)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t192)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv195 string
    retv195 = self__38
    return retv195
}

func main() {
    main0()
}
