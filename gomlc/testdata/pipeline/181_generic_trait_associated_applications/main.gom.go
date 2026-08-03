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

type Value struct {}

func main0() struct{} {
    var text__2 string
    text__2 = "int"
    var number__3 int32
    number__3 = 7
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__2)
    _goml_runtime_core_string_println(inline201)
    var inline198 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(number__3)
    _goml_runtime_core_string_println(inline198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t196 string = _goml_runtime_core_int32_to_string(self__72)
    return t196
}

func main() {
    main0()
}
