package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv157 string
    var jp159 string
    switch n__0 {
    case -1:
        jp159 = "minus one"
    case 0:
        jp159 = "zero"
    case 1:
        jp159 = "one"
    default:
        jp159 = "other"
    }
    retv157 = jp159
    return retv157
}

func main0() struct{} {
    var t161 string = classify(-1)
    println__T_string(t161)
    var t162 string = classify(0)
    println__T_string(t162)
    var t163 string = classify(1)
    println__T_string(t163)
    var t164 string = classify(42)
    println__T_string(t164)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv169 string
    retv169 = self__38
    return retv169
}

func main() {
    main0()
}
