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

type Ordering int32

func _goml_m_trait__impl_i_Source_i_Value_i_get(self__1 Value) int32 {
    var t416 int32 = self__1.value
    return t416
}

func main0() struct{} {
    var t418 Value = Value{
        value: 41,
    }
    var direct__6 int32
    var inline448 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(t418)
    direct__6 = inline448
    var inline445 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(direct__6)
    _goml_runtime_core_string_println(inline445)
    var t419 Value = Value{
        value: 42,
    }
    var t420 int32
    var inline440 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t419)
    var inline441 closure_env_identity_0 = closure_env_identity_0{}
    var inline442 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(inline441, p0)
    }
    var inline443 int32 = inline442(inline440)
    t420 = inline443
    var inline437 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t420)
    _goml_runtime_core_string_println(inline437)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var inline450 int32 = self__0.value
    return inline450
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t433 string = _goml_runtime_core_int32_to_string(self__154)
    return t433
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env413 closure_env_identity_0, item__4 int32) int32 {
    return item__4
}

func main() {
    main0()
}
