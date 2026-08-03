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
    var t182 int32 = self__1.value
    return t182
}

func main0() struct{} {
    var t184 Value = Value{
        value: 41,
    }
    var direct__6 int32
    var inline212 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(t184)
    direct__6 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(direct__6)
    _goml_runtime_core_string_println(inline209)
    var t185 Value = Value{
        value: 42,
    }
    var t186 int32
    var inline205 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t185)
    var inline206 closure_env_identity_0 = closure_env_identity_0{}
    var inline207 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(inline206, inline205)
    t186 = inline207
    var inline202 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t186)
    _goml_runtime_core_string_println(inline202)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var inline214 int32 = self__0.value
    return inline214
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t198 string = _goml_runtime_core_int32_to_string(self__72)
    return t198
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env179 closure_env_identity_0, item__4 int32) int32 {
    return item__4
}

func main() {
    main0()
}
