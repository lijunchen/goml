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
    var mtmp177 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline219 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline220 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: inline219,
        }
        mtmp177 = inline220
    } else {
        var inline221 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        mtmp177 = inline221
    }
    var jp197 Tuple2_6string_6string
    switch mtmp177.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x178 Tuple2_6string_6string = mtmp177.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp197 = x178
        var x181 string = jp197._0
        var x182 string = jp197._1
        var t198 string = x181 + ":"
        var t199 string = t198 + x182
        var t200 Result__string__string = Result__string__string_Ok{
            _0: t199,
        }
        return t200
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x179 string = mtmp177.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t201 Result__string__string = Result__string__string_Err{
            _0: x179,
        }
        return t201
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t209 Result__string__string = render(true)
    var t210 string
    switch t209.(type) {
    case Result__string__string_Ok:
        var inline236 string = t209.(Result__string__string_Ok)._0
        var inline238 string = "ok " + inline236
        t210 = inline238
    case Result__string__string_Err:
        var inline239 string = t209.(Result__string__string_Err)._0
        var inline241 string = "err " + inline239
        t210 = inline241
    default:
        panic("non-exhaustive match")
    }
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline233)
    var t211 Result__string__string = render(false)
    var t212 string
    switch t211.(type) {
    case Result__string__string_Ok:
        var inline226 string = t211.(Result__string__string_Ok)._0
        var inline228 string = "ok " + inline226
        t212 = inline228
    case Result__string__string_Err:
        var inline229 string = t211.(Result__string__string_Err)._0
        var inline231 string = "err " + inline229
        t212 = inline231
    default:
        panic("non-exhaustive match")
    }
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
