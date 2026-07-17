package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv63 string
    var jp65 string
    switch n__0 {
    case -1:
        jp65 = "minus one"
    case 0:
        jp65 = "zero"
    case 1:
        jp65 = "one"
    default:
        jp65 = "other"
    }
    retv63 = jp65
    return retv63
}

func main0() struct{} {
    var t67 string = classify(-1)
    println__T_string(t67)
    var t68 string = classify(0)
    println__T_string(t68)
    var t69 string = classify(1)
    println__T_string(t69)
    var t70 string = classify(42)
    println__T_string(t70)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv75 string
    retv75 = self__34
    return retv75
}

func main() {
    main0()
}
