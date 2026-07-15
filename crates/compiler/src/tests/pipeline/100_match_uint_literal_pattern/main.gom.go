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
    var jp25 string
    switch x__0 {
    case 0:
        jp25 = "zero"
    case 1:
        jp25 = "one"
    default:
        jp25 = "other"
    }
    var y__1 string = jp25
    println__T_string(y__1)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t27 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t27)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv30 string
    retv30 = self__9
    return retv30
}

func main() {
    main0()
}
