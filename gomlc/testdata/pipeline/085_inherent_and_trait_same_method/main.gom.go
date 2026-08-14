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

type Boxed struct {
    value int32
}

func main0() struct{} {
    var t192 string
    t192 = "inherent"
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline212)
    var t194 string
    var inline209 int32 = 9
    var inline210 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline209)
    t194 = inline210
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t197 string = _goml_runtime_core_int32_to_string(self__33)
    return t197
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
