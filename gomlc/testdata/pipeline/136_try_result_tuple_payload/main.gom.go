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

func render(ok__1 bool) Result__string__string {
    var mtmp182 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline224 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline225 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: inline224,
        }
        mtmp182 = inline225
    } else {
        var inline226 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        mtmp182 = inline226
    }
    var jp202 Tuple2_6string_6string
    switch mtmp182.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x183 Tuple2_6string_6string = mtmp182.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp202 = x183
        var x186 string = jp202._0
        var x187 string = jp202._1
        var t203 string = x186 + ":"
        var t204 string = t203 + x187
        var t205 Result__string__string = Result__string__string_Ok{
            _0: t204,
        }
        return t205
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x184 string = mtmp182.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t206 Result__string__string = Result__string__string_Err{
            _0: x184,
        }
        return t206
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t214 Result__string__string = render(true)
    var t215 string
    switch t214.(type) {
    case Result__string__string_Ok:
        var inline241 string = t214.(Result__string__string_Ok)._0
        var inline243 string = "ok " + inline241
        t215 = inline243
    case Result__string__string_Err:
        var inline244 string = t214.(Result__string__string_Err)._0
        var inline246 string = "err " + inline244
        t215 = inline246
    default:
        panic("non-exhaustive match")
    }
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline238)
    var t216 Result__string__string = render(false)
    var t217 string
    switch t216.(type) {
    case Result__string__string_Ok:
        var inline231 string = t216.(Result__string__string_Ok)._0
        var inline233 string = "ok " + inline231
        t217 = inline233
    case Result__string__string_Err:
        var inline234 string = t216.(Result__string__string_Err)._0
        var inline236 string = "err " + inline234
        t217 = inline236
    default:
        panic("non-exhaustive match")
    }
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
