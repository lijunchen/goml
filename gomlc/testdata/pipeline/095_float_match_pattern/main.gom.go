package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv74 string
    var jp76 string
    switch x__0 {
    case 0:
        jp76 = "zero"
    case 1:
        jp76 = "one"
    case -1:
        jp76 = "minus one"
    case 3.14:
        jp76 = "pi"
    default:
        jp76 = "other"
    }
    retv74 = jp76
    return retv74
}

func main0() struct{} {
    var t78 string = classify(0)
    println__T_string(t78)
    var t79 string = classify(1)
    println__T_string(t79)
    var t80 float64 = -1
    var t81 string = classify(t80)
    println__T_string(t81)
    var t82 string = classify(3.14)
    println__T_string(t82)
    var t83 string = classify(42)
    println__T_string(t83)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv88 string
    retv88 = self__38
    return retv88
}

func main() {
    main0()
}
