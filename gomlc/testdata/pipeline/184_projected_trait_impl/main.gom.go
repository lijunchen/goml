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
    var retv156 int32
    var t157 int32 = self__1.value
    retv156 = t157
    return retv156
}

func main0() struct{} {
    var t159 Value = Value{
        value: 41,
    }
    var direct__6 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(t159)
    println__T_int32(direct__6)
    var t160 Value = Value{
        value: 42,
    }
    var t161 int32 = copy__S_Value(t160)
    println__T_int32(t161)
    return struct{}{}
}

func _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(self__0 Value) int32 {
    var retv163 int32
    var t164 int32 = _goml_m_trait__impl_i_Source_i_Value_i_get(self__0)
    retv163 = t164
    return retv163
}

func println__T_int32(value__1 int32) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func copy__S_Value(source__2 Value) int32 {
    var retv169 int32
    var value__3 int32 = _goml_m_trait__impl_i_Pick_i__l_int32_r__x40_Value_i_pick(source__2)
    var identity__5 closure_env_identity_0 = closure_env_identity_0{}
    var t170 int32 = _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(identity__5, value__3)
    retv169 = t170
    return retv169
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv172 string
    var t173 string = _goml_runtime_core_int32_to_string(self__43)
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_closure__env__identity__0_i_closure__env__identity__0_i_apply(env154 closure_env_identity_0, item__4 int32) int32 {
    var retv175 int32
    retv175 = item__4
    return retv175
}

func main() {
    main0()
}
