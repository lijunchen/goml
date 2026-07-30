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
    var retv112 int32
    var t113 int32 = self__1.value
    retv112 = t113
    return retv112
}

func main0() struct{} {
    var t115 Value = Value{
        value: 41,
    }
    var direct__6 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t115)
    println__T_int32(direct__6)
    var t116 Value = Value{
        value: 42,
    }
    var t117 int32 = copy__S_Value(t116)
    println__T_int32(t117)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var retv119 int32
    var t120 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(self__0)
    retv119 = t120
    return retv119
}

func println__T_int32(value__1 int32) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func copy__S_Value(source__2 Value) int32 {
    var retv125 int32
    var value__3 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(source__2)
    var identity__5 closure_env_identity_0 = closure_env_identity_0{}
    var t126 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(identity__5, value__3)
    retv125 = t126
    return retv125
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv128 string
    var t129 string = _goml_runtime_core_int32_to_string(self__43)
    retv128 = t129
    return retv128
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env110 closure_env_identity_0, item__4 int32) int32 {
    var retv131 int32
    retv131 = item__4
    return retv131
}

func main() {
    main0()
}
