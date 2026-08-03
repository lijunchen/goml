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
    var mtmp136 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline178 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline179 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: inline178,
        }
        mtmp136 = inline179
    } else {
        var inline180 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        mtmp136 = inline180
    }
    var jp156 Tuple2_6string_6string
    switch mtmp136.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x137 Tuple2_6string_6string = mtmp136.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp156 = x137
        var x140 string = jp156._0
        var x141 string = jp156._1
        var t157 string = x140 + ":"
        var t158 string = t157 + x141
        var t159 Result__string__string = Result__string__string_Ok{
            _0: t158,
        }
        return t159
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x138 string = mtmp136.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t160 Result__string__string = Result__string__string_Err{
            _0: x138,
        }
        return t160
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t168 Result__string__string = render(true)
    var t169 string
    switch t168.(type) {
    case Result__string__string_Ok:
        var inline195 string = t168.(Result__string__string_Ok)._0
        var inline197 string = "ok " + inline195
        t169 = inline197
    case Result__string__string_Err:
        var inline198 string = t168.(Result__string__string_Err)._0
        var inline200 string = "err " + inline198
        t169 = inline200
    default:
        panic("non-exhaustive match")
    }
    var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
    _goml_runtime_core_string_println(inline192)
    var t170 Result__string__string = render(false)
    var t171 string
    switch t170.(type) {
    case Result__string__string_Ok:
        var inline185 string = t170.(Result__string__string_Ok)._0
        var inline187 string = "ok " + inline185
        t171 = inline187
    case Result__string__string_Err:
        var inline188 string = t170.(Result__string__string_Err)._0
        var inline190 string = "err " + inline188
        t171 = inline190
    default:
        panic("non-exhaustive match")
    }
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
    _goml_runtime_core_string_println(inline182)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
