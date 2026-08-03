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
    var t184 S = S{}
    var t185 int32
    var inline217 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(t184)
    t185 = inline217
    var t186 string
    var inline215 string = _goml_runtime_core_int32_to_string(t185)
    t186 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline212)
    var t187 S = S{}
    var t188 int32
    var inline210 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(t187)
    t188 = inline210
    var t189 string
    var inline208 string = _goml_runtime_core_int32_to_string(t188)
    t189 = inline208
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
