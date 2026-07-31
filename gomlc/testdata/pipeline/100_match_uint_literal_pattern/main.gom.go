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
    var jp155 string
    switch x__0 {
    case 0:
        jp155 = "zero"
    case 1:
        jp155 = "one"
    default:
        jp155 = "other"
    }
    var y__1 string = jp155
    println__T_string(y__1)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t157 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t157)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv160 string
    retv160 = self__38
    return retv160
}

func main() {
    main0()
}
