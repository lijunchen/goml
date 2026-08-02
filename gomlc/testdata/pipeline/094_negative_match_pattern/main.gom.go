package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv160 string
    var jp162 string
    switch n__0 {
    case -1:
        jp162 = "minus one"
    case 0:
        jp162 = "zero"
    case 1:
        jp162 = "one"
    default:
        jp162 = "other"
    }
    retv160 = jp162
    return retv160
}

func main0() struct{} {
    var t164 string = classify(-1)
    println__T_string(t164)
    var t165 string = classify(0)
    println__T_string(t165)
    var t166 string = classify(1)
    println__T_string(t166)
    var t167 string = classify(42)
    println__T_string(t167)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv172 string
    retv172 = self__38
    return retv172
}

func main() {
    main0()
}
