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
    var t189 string
    var inline216 string = "direct"
    t189 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline213)
    var t190 int32
    var inline211 int32 = 42
    t190 = inline211
    var t191 string
    var inline209 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline209
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
