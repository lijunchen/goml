package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv12 string
    var jp14 string
    switch n__0 {
    case -1:
        jp14 = "minus one"
    case 0:
        jp14 = "zero"
    case 1:
        jp14 = "one"
    default:
        jp14 = "other"
    }
    retv12 = jp14
    return retv12
}

func main0() struct{} {
    var t16 string = classify(-1)
    println__T_string(t16)
    var t17 string = classify(0)
    println__T_string(t17)
    var t18 string = classify(1)
    println__T_string(t18)
    var t19 string = classify(42)
    println__T_string(t19)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t21 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t21)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv24 string
    retv24 = self__9
    return retv24
}

func main() {
    main0()
}
