package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv70 string
    var jp72 string
    switch x__0 {
    case 0:
        jp72 = "zero"
    case 1:
        jp72 = "one"
    case -1:
        jp72 = "minus one"
    case 3.14:
        jp72 = "pi"
    default:
        jp72 = "other"
    }
    retv70 = jp72
    return retv70
}

func main0() struct{} {
    var t74 string = classify(0)
    println__T_string(t74)
    var t75 string = classify(1)
    println__T_string(t75)
    var t76 float64 = -1
    var t77 string = classify(t76)
    println__T_string(t77)
    var t78 string = classify(3.14)
    println__T_string(t78)
    var t79 string = classify(42)
    println__T_string(t79)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv84 string
    retv84 = self__38
    return retv84
}

func main() {
    main0()
}
