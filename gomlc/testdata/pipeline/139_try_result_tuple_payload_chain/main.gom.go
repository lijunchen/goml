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
        var t150 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t151 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Ok{
            _0: t150,
        }
        return t151
    } else {
        var t152 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string_Err{
            _0: "missing port",
        }
        return t152
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp136 _goml_m_Result_____o_string_c_string_q_____string
    var inline185 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp136 = inline185
    var jp159 Tuple2_6string_6string
    switch mtmp136.(type) {
    case _goml_m_Result_____o_string_c_string_q_____string_Ok:
        var x137 Tuple2_6string_6string = mtmp136.(_goml_m_Result_____o_string_c_string_q_____string_Ok)._0
        jp159 = x137
        var x140 string = jp159._0
        var x141 string = jp159._1
        var t160 string = x140 + "="
        var t161 string = t160 + x141
        var t162 Result__string__string = Result__string__string_Ok{
            _0: t161,
        }
        return t162
    case _goml_m_Result_____o_string_c_string_q_____string_Err:
        var x138 string = mtmp136.(_goml_m_Result_____o_string_c_string_q_____string_Err)._0
        var t163 Result__string__string = Result__string__string_Err{
            _0: x138,
        }
        return t163
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t171 Result__string__string = render(true)
    var t172 string
    switch t171.(type) {
    case Result__string__string_Ok:
        var inline200 string = t171.(Result__string__string_Ok)._0
        var inline202 string = "ok " + inline200
        t172 = inline202
    case Result__string__string_Err:
        var inline203 string = t171.(Result__string__string_Err)._0
        var inline205 string = "err " + inline203
        t172 = inline205
    default:
        panic("non-exhaustive match")
    }
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline197)
    var t173 Result__string__string = render(false)
    var t174 string
    switch t173.(type) {
    case Result__string__string_Ok:
        var inline190 string = t173.(Result__string__string_Ok)._0
        var inline192 string = "ok " + inline190
        t174 = inline192
    case Result__string__string_Err:
        var inline193 string = t173.(Result__string__string_Err)._0
        var inline195 string = "err " + inline193
        t174 = inline195
    default:
        panic("non-exhaustive match")
    }
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
