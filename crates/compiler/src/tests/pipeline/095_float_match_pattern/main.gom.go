package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv10 string
    var jp12 string
    switch x__0 {
    case 0:
        jp12 = "zero"
    case 1:
        jp12 = "one"
    case -1:
        jp12 = "minus one"
    case 3.14:
        jp12 = "pi"
    default:
        jp12 = "other"
    }
    retv10 = jp12
    return retv10
}

func main0() struct{} {
    var t14 string = classify(0)
    println__T_string(t14)
    var t15 string = classify(1)
    println__T_string(t15)
    var t16 float64 = -1
    var t17 string = classify(t16)
    println__T_string(t17)
    var t18 string = classify(3.14)
    println__T_string(t18)
    var t19 string = classify(42)
    println__T_string(t19)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t21 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t21)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv24 string
    retv24 = self__9
    return retv24
}

func main() {
    main0()
}
