package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 float64) string {
    var retv67 string
    var jp69 string
    switch x__0 {
    case 0:
        jp69 = "zero"
    case 1:
        jp69 = "one"
    case -1:
        jp69 = "minus one"
    case 3.14:
        jp69 = "pi"
    default:
        jp69 = "other"
    }
    retv67 = jp69
    return retv67
}

func main0() struct{} {
    var t71 string = classify(0)
    println__T_string(t71)
    var t72 string = classify(1)
    println__T_string(t72)
    var t73 float64 = -1
    var t74 string = classify(t73)
    println__T_string(t74)
    var t75 string = classify(3.14)
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

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv81 string
    retv81 = self__37
    return retv81
}

func main() {
    main0()
}
