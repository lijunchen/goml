package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv69 string
    var jp71 string
    switch n__0 {
    case -1:
        jp71 = "minus one"
    case 0:
        jp71 = "zero"
    case 1:
        jp71 = "one"
    default:
        jp71 = "other"
    }
    retv69 = jp71
    return retv69
}

func main0() struct{} {
    var t73 string = classify(-1)
    println__T_string(t73)
    var t74 string = classify(0)
    println__T_string(t74)
    var t75 string = classify(1)
    println__T_string(t75)
    var t76 string = classify(42)
    println__T_string(t76)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv81 string
    retv81 = self__38
    return retv81
}

func main() {
    main0()
}
