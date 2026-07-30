package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv73 string
    var jp75 string
    switch n__0 {
    case -1:
        jp75 = "minus one"
    case 0:
        jp75 = "zero"
    case 1:
        jp75 = "one"
    default:
        jp75 = "other"
    }
    retv73 = jp75
    return retv73
}

func main0() struct{} {
    var t77 string = classify(-1)
    println__T_string(t77)
    var t78 string = classify(0)
    println__T_string(t78)
    var t79 string = classify(1)
    println__T_string(t79)
    var t80 string = classify(42)
    println__T_string(t80)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv85 string
    retv85 = self__38
    return retv85
}

func main() {
    main0()
}
