package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Value struct {
    value int32
}

type closure_env_identity_0 struct {}

func _goml_m_trait__impl_i_Source_i_Value_i_get(self__1 Value) int32 {
    var t177 int32 = self__1.value
    return t177
}

func main0() struct{} {
    var t179 Value = Value{
        value: 41,
    }
    var direct__6 int32
    var inline207 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(t179)
    direct__6 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(direct__6)
    _goml_runtime_core_string_println(inline204)
    var t180 Value = Value{
        value: 42,
    }
    var t181 int32
    var inline200 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t180)
    var inline201 closure_env_identity_0 = closure_env_identity_0{}
    var inline202 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(inline201, inline200)
    t181 = inline202
    var inline197 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
    _goml_runtime_core_string_println(inline197)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var inline209 int32 = self__0.value
    return inline209
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t193 string = _goml_runtime_core_int32_to_string(self__72)
    return t193
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env174 closure_env_identity_0, item__4 int32) int32 {
    return item__4
}

func main() {
    main0()
}
