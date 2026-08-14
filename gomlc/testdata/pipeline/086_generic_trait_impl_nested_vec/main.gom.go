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
    var t190 int32
    t190 = 1
    var t191 string
    var inline216 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline213)
    var t192 int32
    t192 = 1
    var t193 string
    var inline210 string = _goml_runtime_core_int32_to_string(t192)
    t193 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
