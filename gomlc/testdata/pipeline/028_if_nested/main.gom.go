package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv75 string
    var t78 bool = x__0 < 0
    var jp77 string
    if t78 {
        jp77 = "negative"
    } else {
        var t81 bool = 0 < x__0
        var jp80 string
        if t81 {
            jp80 = "positive"
        } else {
            jp80 = "zero"
        }
        jp77 = jp80
    }
    retv75 = jp77
    return retv75
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv83 string
    var t86 bool = a__1 < b__2
    var jp85 string
    if t86 {
        var t89 bool = b__2 < c__3
        var jp88 string
        if t89 {
            jp88 = "ascending"
        } else {
            jp88 = "peak"
        }
        jp85 = jp88
    } else {
        var t92 bool = a__1 < c__3
        var jp91 string
        if t92 {
            jp91 = "valley"
        } else {
            jp91 = "flat"
        }
        jp85 = jp91
    }
    retv83 = jp85
    return retv83
}

func main0() struct{} {
    var first__4 string = classify(-42)
    var second__5 string = classify(0)
    var third__6 string = classify(17)
    var shape1__7 string = triangle_type(1, 2, 3)
    var shape2__8 string = triangle_type(3, 2, 1)
    var shape3__9 string = triangle_type(2, 3, 2)
    println__T_string(first__4)
    println__T_string(second__5)
    println__T_string(third__6)
    println__T_string(shape1__7)
    println__T_string(shape2__8)
    println__T_string(shape3__9)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv98 string
    retv98 = self__38
    return retv98
}

func main() {
    main0()
}
