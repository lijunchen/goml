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
    var jp61 string
    switch x__0 {
    case 0:
        jp61 = "zero"
    case 1:
        jp61 = "one"
    default:
        jp61 = "other"
    }
    var y__1 string = jp61
    println__T_string(y__1)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t63 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t63)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv66 string
    retv66 = self__34
    return retv66
}

func main() {
    main0()
}
