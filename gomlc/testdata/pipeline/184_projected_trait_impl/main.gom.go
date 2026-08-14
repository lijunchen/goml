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
    var t187 int32 = self__1.value
    return t187
}

func main0() struct{} {
    var t189 Value = Value{
        value: 41,
    }
    var direct__6 int32
    var inline219 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(t189)
    direct__6 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(direct__6)
    _goml_runtime_core_string_println(inline216)
    var t190 Value = Value{
        value: 42,
    }
    var t191 int32
    var inline211 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t190)
    var inline212 closure_env_identity_0 = closure_env_identity_0{}
    var inline213 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(inline212, p0)
    }
    var inline214 int32 = inline213(inline211)
    t191 = inline214
    var inline208 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t191)
    _goml_runtime_core_string_println(inline208)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var inline221 int32 = self__0.value
    return inline221
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t204 string = _goml_runtime_core_int32_to_string(self__70)
    return t204
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env184 closure_env_identity_0, item__4 int32) int32 {
    return item__4
}

func main() {
    main0()
}
