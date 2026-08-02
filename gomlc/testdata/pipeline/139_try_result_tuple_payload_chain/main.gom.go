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
    if ok__0 {
        var t169 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t170 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t169,
        }
        return t170
    } else {
        var t171 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        return t171
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp155 _goml_m_Result_____o_string_c_string_q_____string
    var inline204 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp155 = inline204
    var jp178 Tuple2_6string_6string
    switch mtmp155.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x156 Tuple2_6string_6string = mtmp155.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp178 = x156
        var x159 string = jp178._0
        var x160 string = jp178._1
        var t179 string = x159 + "="
        var t180 string = t179 + x160
        var t181 Result__string__string = Result__string__string_Ok{
            _0: t180,
        }
        return t181
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x157 string = mtmp155.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t182 Result__string__string = Result__string__string_Err{
            _0: x157,
        }
        return t182
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t190 Result__string__string = render(true)
    var t191 string
    switch t190.(type) {
    case Result__string__string_Ok:
        var inline219 string = t190.(Result__string__string_Ok)._0
        var inline221 string = "ok " + inline219
        t191 = inline221
    case Result__string__string_Err:
        var inline222 string = t190.(Result__string__string_Err)._0
        var inline224 string = "err " + inline222
        t191 = inline224
    default:
        panic("non-exhaustive match")
    }
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline216)
    var t192 Result__string__string = render(false)
    var t193 string
    switch t192.(type) {
    case Result__string__string_Ok:
        var inline209 string = t192.(Result__string__string_Ok)._0
        var inline211 string = "ok " + inline209
        t193 = inline211
    case Result__string__string_Err:
        var inline212 string = t192.(Result__string__string_Err)._0
        var inline214 string = "err " + inline212
        t193 = inline214
    default:
        panic("non-exhaustive match")
    }
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
