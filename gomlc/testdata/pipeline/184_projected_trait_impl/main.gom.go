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
    var t413 int32 = self__1.value
    return t413
}

func main0() struct{} {
    var t415 Value = Value{
        value: 41,
    }
    var direct__6 int32
    var inline445 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(t415)
    direct__6 = inline445
    var inline442 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(direct__6)
    _goml_runtime_core_string_println(inline442)
    var t416 Value = Value{
        value: 42,
    }
    var t417 int32
    var inline437 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t416)
    var inline438 closure_env_identity_0 = closure_env_identity_0{}
    var inline439 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(inline438, p0)
    }
    var inline440 int32 = inline439(inline437)
    t417 = inline440
    var inline434 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t417)
    _goml_runtime_core_string_println(inline434)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var inline447 int32 = self__0.value
    return inline447
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t430 string = _goml_runtime_core_int32_to_string(self__154)
    return t430
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env410 closure_env_identity_0, item__4 int32) int32 {
    return item__4
}

func main() {
    main0()
}
