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
        var t196 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t197 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t196,
        }
        return t197
    } else {
        var t198 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        return t198
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp182 _goml_m_Result_____o_string_c_string_q_____string
    var inline231 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp182 = inline231
    var jp205 Tuple2_6string_6string
    switch mtmp182.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x183 Tuple2_6string_6string = mtmp182.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp205 = x183
        var x186 string = jp205._0
        var x187 string = jp205._1
        var t206 string = x186 + "="
        var t207 string = t206 + x187
        var t208 Result__string__string = Result__string__string_Ok{
            _0: t207,
        }
        return t208
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x184 string = mtmp182.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t209 Result__string__string = Result__string__string_Err{
            _0: x184,
        }
        return t209
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t217 Result__string__string = render(true)
    var t218 string
    switch t217.(type) {
    case Result__string__string_Ok:
        var inline246 string = t217.(Result__string__string_Ok)._0
        var inline248 string = "ok " + inline246
        t218 = inline248
    case Result__string__string_Err:
        var inline249 string = t217.(Result__string__string_Err)._0
        var inline251 string = "err " + inline249
        t218 = inline251
    default:
        panic("non-exhaustive match")
    }
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
    _goml_runtime_core_string_println(inline243)
    var t219 Result__string__string = render(false)
    var t220 string
    switch t219.(type) {
    case Result__string__string_Ok:
        var inline236 string = t219.(Result__string__string_Ok)._0
        var inline238 string = "ok " + inline236
        t220 = inline238
    case Result__string__string_Err:
        var inline239 string = t219.(Result__string__string_Err)._0
        var inline241 string = "err " + inline239
        t220 = inline241
    default:
        panic("non-exhaustive match")
    }
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline233)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
