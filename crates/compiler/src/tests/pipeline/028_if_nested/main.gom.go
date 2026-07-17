package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv65 string
    var t68 bool = x__0 < 0
    var jp67 string
    if t68 {
        jp67 = "negative"
    } else {
        var t71 bool = 0 < x__0
        var jp70 string
        if t71 {
            jp70 = "positive"
        } else {
            jp70 = "zero"
        }
        jp67 = jp70
    }
    retv65 = jp67
    return retv65
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv73 string
    var t76 bool = a__1 < b__2
    var jp75 string
    if t76 {
        var t79 bool = b__2 < c__3
        var jp78 string
        if t79 {
            jp78 = "ascending"
        } else {
            jp78 = "peak"
        }
        jp75 = jp78
    } else {
        var t82 bool = a__1 < c__3
        var jp81 string
        if t82 {
            jp81 = "valley"
        } else {
            jp81 = "flat"
        }
        jp75 = jp81
    }
    retv73 = jp75
    return retv73
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
    var t85 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv88 string
    retv88 = self__34
    return retv88
}

func main() {
    main0()
}
