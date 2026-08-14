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
    var jp411 string
    switch x__0 {
    case 0:
        jp411 = "zero"
    case 1:
        jp411 = "one"
    default:
        jp411 = "other"
    }
    var inline418 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp411)
    _goml_runtime_core_string_println(inline418)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
