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

func main0() struct{} {
    var first__3 int32 = 1
    var t191 int32
    var inline212 int32 = _goml_m_trait__impl_i_Extra_i_int32_i_extra(first__3)
    t191 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t191)
    _goml_runtime_core_string_println(inline209)
    var t192 int32
    t192 = 42
    var inline205 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t192)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_Extra_i_int32_i_extra(self__1 int32) int32 {
    return 42
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t203 string = _goml_runtime_core_int32_to_string(self__70)
    return t203
}

func main() {
    main0()
}
