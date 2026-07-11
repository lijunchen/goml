package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv27 string
    var jp29 string
    switch n__0 {
    case -1:
        jp29 = "minus one"
    case 0:
        jp29 = "zero"
    case 1:
        jp29 = "one"
    default:
        jp29 = "other"
    }
    retv27 = jp29
    return retv27
}

func main0() struct{} {
    var t31 string = classify(-1)
    println__T_string(t31)
    var t32 string = classify(0)
    println__T_string(t32)
    var t33 string = classify(1)
    println__T_string(t33)
    var t34 string = classify(42)
    println__T_string(t34)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
