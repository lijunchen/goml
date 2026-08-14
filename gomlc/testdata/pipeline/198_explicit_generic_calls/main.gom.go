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
    var t184 string
    var inline211 string = "direct"
    t184 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline208)
    var t185 int32
    var inline206 int32 = 42
    t185 = inline206
    var t186 string
    var inline204 string = _goml_runtime_core_int32_to_string(t185)
    t186 = inline204
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
