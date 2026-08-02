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
    var mtmp155 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline197 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline198 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: inline197,
        }
        mtmp155 = inline198
    } else {
        var inline199 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        mtmp155 = inline199
    }
    var jp175 Tuple2_6string_6string
    switch mtmp155.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x156 Tuple2_6string_6string = mtmp155.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp175 = x156
        var x159 string = jp175._0
        var x160 string = jp175._1
        var t176 string = x159 + ":"
        var t177 string = t176 + x160
        var t178 Result__string__string = Result__string__string_Ok{
            _0: t177,
        }
        return t178
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x157 string = mtmp155.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t179 Result__string__string = Result__string__string_Err{
            _0: x157,
        }
        return t179
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t187 Result__string__string = render(true)
    var t188 string
    switch t187.(type) {
    case Result__string__string_Ok:
        var inline214 string = t187.(Result__string__string_Ok)._0
        var inline216 string = "ok " + inline214
        t188 = inline216
    case Result__string__string_Err:
        var inline217 string = t187.(Result__string__string_Err)._0
        var inline219 string = "err " + inline217
        t188 = inline219
    default:
        panic("non-exhaustive match")
    }
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline211)
    var t189 Result__string__string = render(false)
    var t190 string
    switch t189.(type) {
    case Result__string__string_Ok:
        var inline204 string = t189.(Result__string__string_Ok)._0
        var inline206 string = "ok " + inline204
        t190 = inline206
    case Result__string__string_Err:
        var inline207 string = t189.(Result__string__string_Err)._0
        var inline209 string = "err " + inline207
        t190 = inline209
    default:
        panic("non-exhaustive match")
    }
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
