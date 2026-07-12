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
    var retv26 int32
    var t27 int32 = self__1.value
    retv26 = t27
    return retv26
}

func main0() struct{} {
    var t29 Value = Value{
        value: 41,
    }
    var direct__6 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t29)
    println__T_int32(direct__6)
    var t30 Value = Value{
        value: 42,
    }
    var t31 int32 = copy__S_Value(t30)
    println__T_int32(t31)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var retv33 int32
    var t34 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(self__0)
    retv33 = t34
    return retv33
}

func println__T_int32(value__1 int32) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func copy__S_Value(source__2 Value) int32 {
    var retv39 int32
    var value__3 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(source__2)
    var identity__5 closure_env_identity_0 = closure_env_identity_0{}
    var t40 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(identity__5, value__3)
    retv39 = t40
    return retv39
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv42 string
    var t43 string = _goml_runtime_core_int32_to_string(self__13)
    retv42 = t43
    return retv42
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env24 closure_env_identity_0, item__4 int32) int32 {
    var retv45 int32
    retv45 = item__4
    return retv45
}

func main() {
    main0()
}
