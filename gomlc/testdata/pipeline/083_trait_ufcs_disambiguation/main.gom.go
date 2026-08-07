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

type S struct {}

func _goml_m_trait__impl_i_A_i_S_i_pick(self__0 S) int32 {
    return 10
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    return 20
}

func main0() struct{} {
    var t179 S = S{}
    var t180 int32
    var inline212 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(t179)
    t180 = inline212
    var t181 string
    var inline210 string = _goml_runtime_core_int32_to_string(t180)
    t181 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline207)
    var t182 S = S{}
    var t183 int32
    var inline205 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(t182)
    t183 = inline205
    var t184 string
    var inline203 string = _goml_runtime_core_int32_to_string(t183)
    t184 = inline203
    var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
