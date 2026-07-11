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
    var jp7 string
    switch x__0 {
    case 0:
        jp7 = "zero"
    case 1:
        jp7 = "one"
    default:
        jp7 = "other"
    }
    var y__1 string = jp7
    println__T_string(y__1)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t9 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv12 string
    retv12 = self__9
    return retv12
}

func main() {
    main0()
}
