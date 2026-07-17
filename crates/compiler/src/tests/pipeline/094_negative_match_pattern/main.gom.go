package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(n__0 int32) string {
    var retv66 string
    var jp68 string
    switch n__0 {
    case -1:
        jp68 = "minus one"
    case 0:
        jp68 = "zero"
    case 1:
        jp68 = "one"
    default:
        jp68 = "other"
    }
    retv66 = jp68
    return retv66
}

func main0() struct{} {
    var t70 string = classify(-1)
    println__T_string(t70)
    var t71 string = classify(0)
    println__T_string(t71)
    var t72 string = classify(1)
    println__T_string(t72)
    var t73 string = classify(42)
    println__T_string(t73)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv78 string
    retv78 = self__37
    return retv78
}

func main() {
    main0()
}
