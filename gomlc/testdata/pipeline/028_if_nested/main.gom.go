package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv162 string
    var t165 bool = x__0 < 0
    var jp164 string
    if t165 {
        jp164 = "negative"
    } else {
        var t168 bool = 0 < x__0
        var jp167 string
        if t168 {
            jp167 = "positive"
        } else {
            jp167 = "zero"
        }
        jp164 = jp167
    }
    retv162 = jp164
    return retv162
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv170 string
    var t173 bool = a__1 < b__2
    var jp172 string
    if t173 {
        var t176 bool = b__2 < c__3
        var jp175 string
        if t176 {
            jp175 = "ascending"
        } else {
            jp175 = "peak"
        }
        jp172 = jp175
    } else {
        var t179 bool = a__1 < c__3
        var jp178 string
        if t179 {
            jp178 = "valley"
        } else {
            jp178 = "flat"
        }
        jp172 = jp178
    }
    retv170 = jp172
    return retv170
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
    var t182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t182)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv185 string
    retv185 = self__38
    return retv185
}

func main() {
    main0()
}
