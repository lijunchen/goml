package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv71 string
    var t74 bool = x__0 < 0
    var jp73 string
    if t74 {
        jp73 = "negative"
    } else {
        var t77 bool = 0 < x__0
        var jp76 string
        if t77 {
            jp76 = "positive"
        } else {
            jp76 = "zero"
        }
        jp73 = jp76
    }
    retv71 = jp73
    return retv71
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv79 string
    var t82 bool = a__1 < b__2
    var jp81 string
    if t82 {
        var t85 bool = b__2 < c__3
        var jp84 string
        if t85 {
            jp84 = "ascending"
        } else {
            jp84 = "peak"
        }
        jp81 = jp84
    } else {
        var t88 bool = a__1 < c__3
        var jp87 string
        if t88 {
            jp87 = "valley"
        } else {
            jp87 = "flat"
        }
        jp81 = jp87
    }
    retv79 = jp81
    return retv79
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
    var t91 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv94 string
    retv94 = self__38
    return retv94
}

func main() {
    main0()
}
