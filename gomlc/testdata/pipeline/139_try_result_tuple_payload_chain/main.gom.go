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
        var t191 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t192 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t191,
        }
        return t192
    } else {
        var t193 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        return t193
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp177 _goml_m_Result_____o_string_c_string_q_____string
    var inline226 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp177 = inline226
    var jp200 Tuple2_6string_6string
    switch mtmp177.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x178 Tuple2_6string_6string = mtmp177.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp200 = x178
        var x181 string = jp200._0
        var x182 string = jp200._1
        var t201 string = x181 + "="
        var t202 string = t201 + x182
        var t203 Result__string__string = Result__string__string_Ok{
            _0: t202,
        }
        return t203
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x179 string = mtmp177.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t204 Result__string__string = Result__string__string_Err{
            _0: x179,
        }
        return t204
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t212 Result__string__string = render(true)
    var t213 string
    switch t212.(type) {
    case Result__string__string_Ok:
        var inline241 string = t212.(Result__string__string_Ok)._0
        var inline243 string = "ok " + inline241
        t213 = inline243
    case Result__string__string_Err:
        var inline244 string = t212.(Result__string__string_Err)._0
        var inline246 string = "err " + inline244
        t213 = inline246
    default:
        panic("non-exhaustive match")
    }
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline238)
    var t214 Result__string__string = render(false)
    var t215 string
    switch t214.(type) {
    case Result__string__string_Ok:
        var inline231 string = t214.(Result__string__string_Ok)._0
        var inline233 string = "ok " + inline231
        t215 = inline233
    case Result__string__string_Err:
        var inline234 string = t214.(Result__string__string_Err)._0
        var inline236 string = "err " + inline234
        t215 = inline236
    default:
        panic("non-exhaustive match")
    }
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
