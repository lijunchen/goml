package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv158 string
    var jp160 string
    switch x__0 {
    case 0:
        jp160 = "zero"
    case 1:
        jp160 = "one"
    case -1:
        jp160 = "minus one"
    case 3.14:
        jp160 = "pi"
    default:
        jp160 = "other"
    }
    retv158 = jp160
    return retv158
}

func main0() struct{} {
    var t162 string = classify(0)
    println__T_string(t162)
    var t163 string = classify(1)
    println__T_string(t163)
    var t164 float64 = -1
    var t165 string = classify(t164)
    println__T_string(t165)
    var t166 string = classify(3.14)
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
