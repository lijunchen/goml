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
    var t179 string
    var inline206 string = "direct"
    t179 = inline206
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
    _goml_runtime_core_string_println(inline203)
    var t180 int32
    var inline201 int32 = 42
    t180 = inline201
    var t181 string
    var inline199 string = _goml_runtime_core_int32_to_string(t180)
    t181 = inline199
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
