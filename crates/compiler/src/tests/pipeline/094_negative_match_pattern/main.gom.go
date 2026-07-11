package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv9 string
    var jp11 string
    switch n__0 {
    case -1:
        jp11 = "minus one"
    case 0:
        jp11 = "zero"
    case 1:
        jp11 = "one"
    default:
        jp11 = "other"
    }
    retv9 = jp11
    return retv9
}

func main0() struct{} {
    var t13 string = classify(-1)
    println__T_string(t13)
    var t14 string = classify(0)
    println__T_string(t14)
    var t15 string = classify(1)
    println__T_string(t15)
    var t16 string = classify(42)
    println__T_string(t16)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t18 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t18)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv21 string
    retv21 = self__9
    return retv21
}

func main() {
    main0()
}
