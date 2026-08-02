package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv161 string
    var jp163 string
    switch x__0 {
    case 0:
        jp163 = "zero"
    case 1:
        jp163 = "one"
    case -1:
        jp163 = "minus one"
    case 3.14:
        jp163 = "pi"
    default:
        jp163 = "other"
    }
    retv161 = jp163
    return retv161
}

func main0() struct{} {
    var t165 string = classify(0)
    println__T_string(t165)
    var t166 string = classify(1)
    println__T_string(t166)
    var t167 float64 = -1
    var t168 string = classify(t167)
    println__T_string(t168)
    var t169 string = classify(3.14)
    println__T_string(t169)
    var t170 string = classify(42)
    println__T_string(t170)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv175 string
    retv175 = self__38
    return retv175
}

func main() {
    main0()
}
