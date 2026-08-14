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
    var t192 int32 = self__1.value
    return t192
}

func main0() struct{} {
    var t194 Value = Value{
        value: 41,
    }
    var direct__6 int32
    var inline224 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(t194)
    direct__6 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(direct__6)
    _goml_runtime_core_string_println(inline221)
    var t195 Value = Value{
        value: 42,
    }
    var t196 int32
    var inline216 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t195)
    var inline217 closure_env_identity_0 = closure_env_identity_0{}
    var inline218 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(inline217, p0)
    }
    var inline219 int32 = inline218(inline216)
    t196 = inline219
    var inline213 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t196)
    _goml_runtime_core_string_println(inline213)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var inline226 int32 = self__0.value
    return inline226
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t209 string = _goml_runtime_core_int32_to_string(self__70)
    return t209
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env189 closure_env_identity_0, item__4 int32) int32 {
    return item__4
}

func main() {
    main0()
}
