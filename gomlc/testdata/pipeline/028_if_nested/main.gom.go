package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv159 string
    var t162 bool = x__0 < 0
    var jp161 string
    if t162 {
        jp161 = "negative"
    } else {
        var t165 bool = 0 < x__0
        var jp164 string
        if t165 {
            jp164 = "positive"
        } else {
            jp164 = "zero"
        }
        jp161 = jp164
    }
    retv159 = jp161
    return retv159
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv167 string
    var t170 bool = a__1 < b__2
    var jp169 string
    if t170 {
        var t173 bool = b__2 < c__3
        var jp172 string
        if t173 {
            jp172 = "ascending"
        } else {
            jp172 = "peak"
        }
        jp169 = jp172
    } else {
        var t176 bool = a__1 < c__3
        var jp175 string
        if t176 {
            jp175 = "valley"
        } else {
            jp175 = "flat"
        }
        jp169 = jp175
    }
    retv167 = jp169
    return retv167
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
    var t179 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t179)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv182 string
    retv182 = self__38
    return retv182
}

func main() {
    main0()
}
