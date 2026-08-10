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
    var jp175 string
    switch x__0 {
    case 0:
        jp175 = "zero"
    case 1:
        jp175 = "one"
    default:
        jp175 = "other"
    }
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp175)
    _goml_runtime_core_string_println(inline182)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
