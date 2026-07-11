package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv28 string
    var jp30 string
    switch x__0 {
    case 0:
        jp30 = "zero"
    case 1:
        jp30 = "one"
    case -1:
        jp30 = "minus one"
    case 3.14:
        jp30 = "pi"
    default:
        jp30 = "other"
    }
    retv28 = jp30
    return retv28
}

func main0() struct{} {
    var t32 string = classify(0)
    println__T_string(t32)
    var t33 string = classify(1)
    println__T_string(t33)
    var t34 float64 = -1
    var t35 string = classify(t34)
    println__T_string(t35)
    var t36 string = classify(3.14)
    println__T_string(t36)
    var t37 string = classify(42)
    println__T_string(t37)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t39 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t39)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv42 string
    retv42 = self__9
    return retv42
}

func main() {
    main0()
}
