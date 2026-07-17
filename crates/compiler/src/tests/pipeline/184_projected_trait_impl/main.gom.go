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
    var retv62 int32
    var t63 int32 = self__1.value
    retv62 = t63
    return retv62
}

func main0() struct{} {
    var t65 Value = Value{
        value: 41,
    }
    var direct__6 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t65)
    println__T_int32(direct__6)
    var t66 Value = Value{
        value: 42,
    }
    var t67 int32 = copy__S_Value(t66)
    println__T_int32(t67)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var retv69 int32
    var t70 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(self__0)
    retv69 = t70
    return retv69
}

func println__T_int32(value__1 int32) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func copy__S_Value(source__2 Value) int32 {
    var retv75 int32
    var value__3 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(source__2)
    var identity__5 closure_env_identity_0 = closure_env_identity_0{}
    var t76 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(identity__5, value__3)
    retv75 = t76
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__38)
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env60 closure_env_identity_0, item__4 int32) int32 {
    var retv81 int32
    retv81 = item__4
    return retv81
}

func main() {
    main0()
}
