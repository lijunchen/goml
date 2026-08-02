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
    var t157 string
    var inline184 string = "direct"
    t157 = inline184
    var inline181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline181)
    var t158 int32
    var inline179 int32 = 42
    t158 = inline179
    var t159 string
    var inline177 string = _goml_runtime_core_int32_to_string(t158)
    t159 = inline177
    var inline174 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline174)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
