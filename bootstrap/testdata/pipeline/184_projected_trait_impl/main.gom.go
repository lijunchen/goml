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
    var retv68 int32
    var t69 int32 = self__1.value
    retv68 = t69
    return retv68
}

func main0() struct{} {
    var t71 Value = Value{
        value: 41,
    }
    var direct__6 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t71)
    println__T_int32(direct__6)
    var t72 Value = Value{
        value: 42,
    }
    var t73 int32 = copy__S_Value(t72)
    println__T_int32(t73)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var retv75 int32
    var t76 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(self__0)
    retv75 = t76
    return retv75
}

func println__T_int32(value__1 int32) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func copy__S_Value(source__2 Value) int32 {
    var retv81 int32
    var value__3 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(source__2)
    var identity__5 closure_env_identity_0 = closure_env_identity_0{}
    var t82 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(identity__5, value__3)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__43)
    retv84 = t85
    return retv84
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env66 closure_env_identity_0, item__4 int32) int32 {
    var retv87 int32
    retv87 = item__4
    return retv87
}

func main() {
    main0()
}
