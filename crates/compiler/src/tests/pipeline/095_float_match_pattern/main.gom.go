package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv13 string
    var jp15 string
    switch x__0 {
    case 0:
        jp15 = "zero"
    case 1:
        jp15 = "one"
    case -1:
        jp15 = "minus one"
    case 3.14:
        jp15 = "pi"
    default:
        jp15 = "other"
    }
    retv13 = jp15
    return retv13
}

func main0() struct{} {
    var t17 string = classify(0)
    println__T_string(t17)
    var t18 string = classify(1)
    println__T_string(t18)
    var t19 float64 = -1
    var t20 string = classify(t19)
    println__T_string(t20)
    var t21 string = classify(3.14)
    println__T_string(t21)
    var t22 string = classify(42)
    println__T_string(t22)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t24 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t24)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv27 string
    retv27 = self__9
    return retv27
}

func main() {
    main0()
}
