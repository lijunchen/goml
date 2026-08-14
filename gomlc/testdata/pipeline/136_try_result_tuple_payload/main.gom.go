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
    var mtmp187 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline229 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline230 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: inline229,
        }
        mtmp187 = inline230
    } else {
        var inline231 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        mtmp187 = inline231
    }
    var jp207 Tuple2_6string_6string
    switch mtmp187.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x188 Tuple2_6string_6string = mtmp187.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp207 = x188
        var x191 string = jp207._0
        var x192 string = jp207._1
        var t208 string = x191 + ":"
        var t209 string = t208 + x192
        var t210 Result__string__string = Result__string__string_Ok{
            _0: t209,
        }
        return t210
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x189 string = mtmp187.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t211 Result__string__string = Result__string__string_Err{
            _0: x189,
        }
        return t211
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t219 Result__string__string = render(true)
    var t220 string
    switch t219.(type) {
    case Result__string__string_Ok:
        var inline246 string = t219.(Result__string__string_Ok)._0
        var inline248 string = "ok " + inline246
        t220 = inline248
    case Result__string__string_Err:
        var inline249 string = t219.(Result__string__string_Err)._0
        var inline251 string = "err " + inline249
        t220 = inline251
    default:
        panic("non-exhaustive match")
    }
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t220)
    _goml_runtime_core_string_println(inline243)
    var t221 Result__string__string = render(false)
    var t222 string
    switch t221.(type) {
    case Result__string__string_Ok:
        var inline236 string = t221.(Result__string__string_Ok)._0
        var inline238 string = "ok " + inline236
        t222 = inline238
    case Result__string__string_Err:
        var inline239 string = t221.(Result__string__string_Err)._0
        var inline241 string = "err " + inline239
        t222 = inline241
    default:
        panic("non-exhaustive match")
    }
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline233)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
