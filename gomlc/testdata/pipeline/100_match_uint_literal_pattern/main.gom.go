package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var x__0 uint8 = 5
    var jp139 string
    switch x__0 {
    case 0:
        jp139 = "zero"
    case 1:
        jp139 = "one"
    default:
        jp139 = "other"
    }
    var inline146 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp139)
    _goml_runtime_core_string_println(inline146)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
