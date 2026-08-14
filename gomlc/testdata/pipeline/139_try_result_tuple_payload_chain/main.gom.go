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
        var t201 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t202 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t201,
        }
        return t202
    } else {
        var t203 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        return t203
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp187 _goml_m_Result_____o_string_c_string_q_____string
    var inline236 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp187 = inline236
    var jp210 Tuple2_6string_6string
    switch mtmp187.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x188 Tuple2_6string_6string = mtmp187.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp210 = x188
        var x191 string = jp210._0
        var x192 string = jp210._1
        var t211 string = x191 + "="
        var t212 string = t211 + x192
        var t213 Result__string__string = Result__string__string_Ok{
            _0: t212,
        }
        return t213
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x189 string = mtmp187.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t214 Result__string__string = Result__string__string_Err{
            _0: x189,
        }
        return t214
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t222 Result__string__string = render(true)
    var t223 string
    switch t222.(type) {
    case Result__string__string_Ok:
        var inline251 string = t222.(Result__string__string_Ok)._0
        var inline253 string = "ok " + inline251
        t223 = inline253
    case Result__string__string_Err:
        var inline254 string = t222.(Result__string__string_Err)._0
        var inline256 string = "err " + inline254
        t223 = inline256
    default:
        panic("non-exhaustive match")
    }
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t223)
    _goml_runtime_core_string_println(inline248)
    var t224 Result__string__string = render(false)
    var t225 string
    switch t224.(type) {
    case Result__string__string_Ok:
        var inline241 string = t224.(Result__string__string_Ok)._0
        var inline243 string = "ok " + inline241
        t225 = inline243
    case Result__string__string_Err:
        var inline244 string = t224.(Result__string__string_Err)._0
        var inline246 string = "err " + inline244
        t225 = inline246
    default:
        panic("non-exhaustive match")
    }
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline238)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
