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
    var t139 int32
    t139 = 1
    var t140 string
    var inline165 string = _goml_runtime_core_int32_to_string(t139)
    t140 = inline165
    var inline162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t140)
    _goml_runtime_core_string_println(inline162)
    var t141 int32
    t141 = 1
    var t142 string
    var inline159 string = _goml_runtime_core_int32_to_string(t141)
    t142 = inline159
    var inline156 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t142)
    _goml_runtime_core_string_println(inline156)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
