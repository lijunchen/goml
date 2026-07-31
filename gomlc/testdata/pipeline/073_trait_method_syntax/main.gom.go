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

type S struct {
    value int32
}

func _goml_m_trait__impl_i_ToString_i_S_i_to__string(self__0 S) string {
    var retv154 string
    var t155 int32 = self__0.value
    var t156 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t155)
    var t157 string = "S(" + t156
    var t158 string = t157 + ")"
    retv154 = t158
    return retv154
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t160 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t160)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv162 string
    var t163 string = _goml_runtime_core_int32_to_string(self__6)
    retv162 = t163
    return retv162
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv168 string
    retv168 = self__38
    return retv168
}

func main() {
    main0()
}
