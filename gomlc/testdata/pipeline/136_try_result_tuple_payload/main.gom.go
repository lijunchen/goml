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
    var mtmp172 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline214 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline215 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: inline214,
        }
        mtmp172 = inline215
    } else {
        var inline216 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        mtmp172 = inline216
    }
    var jp192 Tuple2_6string_6string
    switch mtmp172.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x173 Tuple2_6string_6string = mtmp172.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp192 = x173
        var x176 string = jp192._0
        var x177 string = jp192._1
        var t193 string = x176 + ":"
        var t194 string = t193 + x177
        var t195 Result__string__string = Result__string__string_Ok{
            _0: t194,
        }
        return t195
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x174 string = mtmp172.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t196 Result__string__string = Result__string__string_Err{
            _0: x174,
        }
        return t196
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t204 Result__string__string = render(true)
    var t205 string
    switch t204.(type) {
    case Result__string__string_Ok:
        var inline231 string = t204.(Result__string__string_Ok)._0
        var inline233 string = "ok " + inline231
        t205 = inline233
    case Result__string__string_Err:
        var inline234 string = t204.(Result__string__string_Err)._0
        var inline236 string = "err " + inline234
        t205 = inline236
    default:
        panic("non-exhaustive match")
    }
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline228)
    var t206 Result__string__string = render(false)
    var t207 string
    switch t206.(type) {
    case Result__string__string_Ok:
        var inline221 string = t206.(Result__string__string_Ok)._0
        var inline223 string = "ok " + inline221
        t207 = inline223
    case Result__string__string_Err:
        var inline224 string = t206.(Result__string__string_Err)._0
        var inline226 string = "err " + inline224
        t207 = inline226
    default:
        panic("non-exhaustive match")
    }
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
