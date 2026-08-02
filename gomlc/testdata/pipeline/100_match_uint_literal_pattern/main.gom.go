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
    var jp158 string
    switch x__0 {
    case 0:
        jp158 = "zero"
    case 1:
        jp158 = "one"
    default:
        jp158 = "other"
    }
    var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp158)
    _goml_runtime_core_string_println(inline165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
