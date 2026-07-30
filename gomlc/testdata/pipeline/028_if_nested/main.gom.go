package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv115 string
    var t118 bool = x__0 < 0
    var jp117 string
    if t118 {
        jp117 = "negative"
    } else {
        var t121 bool = 0 < x__0
        var jp120 string
        if t121 {
            jp120 = "positive"
        } else {
            jp120 = "zero"
        }
        jp117 = jp120
    }
    retv115 = jp117
    return retv115
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv123 string
    var t126 bool = a__1 < b__2
    var jp125 string
    if t126 {
        var t129 bool = b__2 < c__3
        var jp128 string
        if t129 {
            jp128 = "ascending"
        } else {
            jp128 = "peak"
        }
        jp125 = jp128
    } else {
        var t132 bool = a__1 < c__3
        var jp131 string
        if t132 {
            jp131 = "valley"
        } else {
            jp131 = "flat"
        }
        jp125 = jp131
    }
    retv123 = jp125
    return retv123
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
    var t135 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t135)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv138 string
    retv138 = self__38
    return retv138
}

func main() {
    main0()
}
