package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var x__0 uint8 = 5
    var jp414 string
    switch x__0 {
    case 0:
        jp414 = "zero"
    case 1:
        jp414 = "one"
    default:
        jp414 = "other"
    }
    var inline421 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp414)
    _goml_runtime_core_string_println(inline421)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
