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
    var retv72 int32
    var t73 int32 = self__1.value
    retv72 = t73
    return retv72
}

func main0() struct{} {
    var t75 Value = Value{
        value: 41,
    }
    var direct__6 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t75)
    println__T_int32(direct__6)
    var t76 Value = Value{
        value: 42,
    }
    var t77 int32 = copy__S_Value(t76)
    println__T_int32(t77)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var retv79 int32
    var t80 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(self__0)
    retv79 = t80
    return retv79
}

func println__T_int32(value__1 int32) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func copy__S_Value(source__2 Value) int32 {
    var retv85 int32
    var value__3 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(source__2)
    var identity__5 closure_env_identity_0 = closure_env_identity_0{}
    var t86 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(identity__5, value__3)
    retv85 = t86
    return retv85
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv88 string
    var t89 string = _goml_runtime_core_int32_to_string(self__43)
    retv88 = t89
    return retv88
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env70 closure_env_identity_0, item__4 int32) int32 {
    var retv91 int32
    retv91 = item__4
    return retv91
}

func main() {
    main0()
}
