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

type Wrap__int struct {
    value int
}

type Wrap__string struct {
    value string
}

func main0() struct{} {
    var t185 int32
    t185 = 1
    var t186 string
    var inline211 string = _goml_runtime_core_int32_to_string(t185)
    t186 = inline211
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline208)
    var t187 int32
    t187 = 1
    var t188 string
    var inline205 string = _goml_runtime_core_int32_to_string(t187)
    t188 = inline205
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
