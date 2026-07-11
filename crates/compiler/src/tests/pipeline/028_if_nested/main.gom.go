package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv14 string
    var t17 bool = x__0 < 0
    var jp16 string
    if t17 {
        jp16 = "negative"
    } else {
        var t20 bool = 0 < x__0
        var jp19 string
        if t20 {
            jp19 = "positive"
        } else {
            jp19 = "zero"
        }
        jp16 = jp19
    }
    retv14 = jp16
    return retv14
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv22 string
    var t25 bool = a__1 < b__2
    var jp24 string
    if t25 {
        var t28 bool = b__2 < c__3
        var jp27 string
        if t28 {
            jp27 = "ascending"
        } else {
            jp27 = "peak"
        }
        jp24 = jp27
    } else {
        var t31 bool = a__1 < c__3
        var jp30 string
        if t31 {
            jp30 = "valley"
        } else {
            jp30 = "flat"
        }
        jp24 = jp30
    }
    retv22 = jp24
    return retv22
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
    var t34 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t34)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv37 string
    retv37 = self__9
    return retv37
}

func main() {
    main0()
}
