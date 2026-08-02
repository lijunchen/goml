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
    var t160 int32 = self__1.value
    return t160
}

func main0() struct{} {
    var t162 Value = Value{
        value: 41,
    }
    var direct__6 int32
    var inline190 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(t162)
    direct__6 = inline190
    var inline187 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(direct__6)
    _goml_runtime_core_string_println(inline187)
    var t163 Value = Value{
        value: 42,
    }
    var t164 int32
    var inline183 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t163)
    var inline184 closure_env_identity_0 = closure_env_identity_0{}
    var inline185 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(inline184, inline183)
    t164 = inline185
    var inline180 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t164)
    _goml_runtime_core_string_println(inline180)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var inline192 int32 = self__0.value
    return inline192
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t176 string = _goml_runtime_core_int32_to_string(self__43)
    return t176
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env157 closure_env_identity_0, item__4 int32) int32 {
    return item__4
}

func main() {
    main0()
}
