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
        var t186 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t187 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t186,
        }
        return t187
    } else {
        var t188 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        return t188
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp172 _goml_m_Result_____o_string_c_string_q_____string
    var inline221 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp172 = inline221
    var jp195 Tuple2_6string_6string
    switch mtmp172.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x173 Tuple2_6string_6string = mtmp172.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp195 = x173
        var x176 string = jp195._0
        var x177 string = jp195._1
        var t196 string = x176 + "="
        var t197 string = t196 + x177
        var t198 Result__string__string = Result__string__string_Ok{
            _0: t197,
        }
        return t198
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x174 string = mtmp172.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t199 Result__string__string = Result__string__string_Err{
            _0: x174,
        }
        return t199
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t207 Result__string__string = render(true)
    var t208 string
    switch t207.(type) {
    case Result__string__string_Ok:
        var inline236 string = t207.(Result__string__string_Ok)._0
        var inline238 string = "ok " + inline236
        t208 = inline238
    case Result__string__string_Err:
        var inline239 string = t207.(Result__string__string_Err)._0
        var inline241 string = "err " + inline239
        t208 = inline241
    default:
        panic("non-exhaustive match")
    }
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline233)
    var t209 Result__string__string = render(false)
    var t210 string
    switch t209.(type) {
    case Result__string__string_Ok:
        var inline226 string = t209.(Result__string__string_Ok)._0
        var inline228 string = "ok " + inline226
        t210 = inline228
    case Result__string__string_Err:
        var inline229 string = t209.(Result__string__string_Err)._0
        var inline231 string = "err " + inline229
        t210 = inline231
    default:
        panic("non-exhaustive match")
    }
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
