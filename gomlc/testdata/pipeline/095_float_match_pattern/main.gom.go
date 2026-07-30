package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv114 string
    var jp116 string
    switch x__0 {
    case 0:
        jp116 = "zero"
    case 1:
        jp116 = "one"
    case -1:
        jp116 = "minus one"
    case 3.14:
        jp116 = "pi"
    default:
        jp116 = "other"
    }
    retv114 = jp116
    return retv114
}

func main0() struct{} {
    var t118 string = classify(0)
    println__T_string(t118)
    var t119 string = classify(1)
    println__T_string(t119)
    var t120 float64 = -1
    var t121 string = classify(t120)
    println__T_string(t121)
    var t122 string = classify(3.14)
    println__T_string(t122)
    var t123 string = classify(42)
    println__T_string(t123)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t125 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t125)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv128 string
    retv128 = self__38
    return retv128
}

func main() {
    main0()
}
