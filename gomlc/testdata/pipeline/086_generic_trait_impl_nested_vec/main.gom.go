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
    var t180 int32
    t180 = 1
    var t181 string
    var inline206 string = _goml_runtime_core_int32_to_string(t180)
    t181 = inline206
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline203)
    var t182 int32
    t182 = 1
    var t183 string
    var inline200 string = _goml_runtime_core_int32_to_string(t182)
    t183 = inline200
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
