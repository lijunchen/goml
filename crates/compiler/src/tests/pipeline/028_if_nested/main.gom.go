package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func classify(x__0 int32) string {
    var retv29 string
    var t32 bool = x__0 < 0
    var jp31 string
    if t32 {
        jp31 = "negative"
    } else {
        var t35 bool = 0 < x__0
        var jp34 string
        if t35 {
            jp34 = "positive"
        } else {
            jp34 = "zero"
        }
        jp31 = jp34
    }
    retv29 = jp31
    return retv29
}

func triangle_type(a__1 int32, b__2 int32, c__3 int32) string {
    var retv37 string
    var t40 bool = a__1 < b__2
    var jp39 string
    if t40 {
        var t43 bool = b__2 < c__3
        var jp42 string
        if t43 {
            jp42 = "ascending"
        } else {
            jp42 = "peak"
        }
        jp39 = jp42
    } else {
        var t46 bool = a__1 < c__3
        var jp45 string
        if t46 {
            jp45 = "valley"
        } else {
            jp45 = "flat"
        }
        jp39 = jp45
    }
    retv37 = jp39
    return retv37
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
    var t49 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t49)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv52 string
    retv52 = self__9
    return retv52
}

func main() {
    main0()
}
