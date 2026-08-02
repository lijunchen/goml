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
    var retv157 string
    var t158 int32 = self__0.value
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t158)
    var t160 string = "S(" + t159
    var t161 string = t160 + ")"
    retv157 = t161
    return retv157
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t163 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t163)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv165 string
    var t166 string = _goml_runtime_core_int32_to_string(self__6)
    retv165 = t166
    return retv165
}

func println__T_string(value__1 string) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv171 string
    retv171 = self__38
    return retv171
}

func main() {
    main0()
}
