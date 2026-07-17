package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv64 string
    var jp66 string
    switch x__0 {
    case 0:
        jp66 = "zero"
    case 1:
        jp66 = "one"
    case -1:
        jp66 = "minus one"
    case 3.14:
        jp66 = "pi"
    default:
        jp66 = "other"
    }
    retv64 = jp66
    return retv64
}

func main0() struct{} {
    var t68 string = classify(0)
    println__T_string(t68)
    var t69 string = classify(1)
    println__T_string(t69)
    var t70 float64 = -1
    var t71 string = classify(t70)
    println__T_string(t71)
    var t72 string = classify(3.14)
    println__T_string(t72)
    var t73 string = classify(42)
    println__T_string(t73)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv78 string
    retv78 = self__34
    return retv78
}

func main() {
    main0()
}
