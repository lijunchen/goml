package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv68 string
    var t71 bool = x__0 < 0
    var jp70 string
    if t71 {
        jp70 = "negative"
    } else {
        var t74 bool = 0 < x__0
        var jp73 string
        if t74 {
            jp73 = "positive"
        } else {
            jp73 = "zero"
        }
        jp70 = jp73
    }
    retv68 = jp70
    return retv68
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv76 string
    var t79 bool = a__1 < b__2
    var jp78 string
    if t79 {
        var t82 bool = b__2 < c__3
        var jp81 string
        if t82 {
            jp81 = "ascending"
        } else {
            jp81 = "peak"
        }
        jp78 = jp81
    } else {
        var t85 bool = a__1 < c__3
        var jp84 string
        if t85 {
            jp84 = "valley"
        } else {
            jp84 = "flat"
        }
        jp78 = jp84
    }
    retv76 = jp78
    return retv76
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
    var t88 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv91 string
    retv91 = self__37
    return retv91
}

func main() {
    main0()
}
