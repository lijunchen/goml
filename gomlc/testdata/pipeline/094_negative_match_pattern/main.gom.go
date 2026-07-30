package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv113 string
    var jp115 string
    switch n__0 {
    case -1:
        jp115 = "minus one"
    case 0:
        jp115 = "zero"
    case 1:
        jp115 = "one"
    default:
        jp115 = "other"
    }
    retv113 = jp115
    return retv113
}

func main0() struct{} {
    var t117 string = classify(-1)
    println__T_string(t117)
    var t118 string = classify(0)
    println__T_string(t118)
    var t119 string = classify(1)
    println__T_string(t119)
    var t120 string = classify(42)
    println__T_string(t120)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv125 string
    retv125 = self__38
    return retv125
}

func main() {
    main0()
}
