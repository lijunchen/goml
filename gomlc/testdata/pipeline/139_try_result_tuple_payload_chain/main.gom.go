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
    var retv166 _goml_m_Result_____o_string_c_string_q_____string
    var jp168 _goml_m_Result_____o_string_c_string_q_____string
    if ok__0 {
        var t169 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t170 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t169,
        }
        jp168 = t170
    } else {
        var t171 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        jp168 = t171
    }
    retv166 = jp168
    return retv166
}

func pair(ok__1 bool) _goml_m_Result_____o_string_c_string_q_____string {
    var retv173 _goml_m_Result_____o_string_c_string_q_____string
    var t174 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__1)
    retv173 = t174
    return retv173
}

func render(ok__2 bool) Result__string__string {
    var retv176 Result__string__string
    var mtmp155 _goml_m_Result_____o_string_c_string_q_____string = pair(ok__2)
    var jp178 Tuple2_6string_6string
    switch mtmp155.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x156 Tuple2_6string_6string = mtmp155.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        var try_value__17 Tuple2_6string_6string = x156
        jp178 = try_value__17
        var mtmp158 Tuple2_6string_6string = jp178
        var x159 string = mtmp158._0
        var x160 string = mtmp158._1
        var port__4 string = x160
        var host__3 string = x159
        var t179 string = host__3 + "="
        var t180 string = t179 + port__4
        var t181 Result__string__string = Result__string__string_Ok{
            _0: t180,
        }
        retv176 = t181
        return retv176
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x157 string = mtmp155.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var try_residual__17 string = x157
        var t182 Result__string__string = Result__string__string_Err{
            _0: try_residual__17,
        }
        retv176 = t182
        return retv176
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv184 string
    var jp186 string
    switch res__5.(type) {
    case Result__string__string_Ok:
        var x161 string = res__5.(Result__string__string_Ok)._0
        var value__6 string = x161
        var t187 string = "ok " + value__6
        jp186 = t187
    case Result__string__string_Err:
        var x162 string = res__5.(Result__string__string_Err)._0
        var err__7 string = x162
        var t188 string = "err " + err__7
        jp186 = t188
    default:
        panic("non-exhaustive match")
    }
    retv184 = jp186
    return retv184
}

func main0() struct{} {
    var t190 Result__string__string = render(true)
    var t191 string = show(t190)
    println__T_string(t191)
    var t192 Result__string__string = render(false)
    var t193 string = show(t192)
    println__T_string(t193)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv198 string
    retv198 = self__38
    return retv198
}

func main() {
    main0()
}
