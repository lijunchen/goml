package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Point struct {
    x int32
    y int32
}

type Maybe__int32 interface {
    isMaybe__int32()
}

type Just struct {
    _0 int32
}

func (_ Just) isMaybe__int32() {}

type Nothing struct {}

func (_ Nothing) isMaybe__int32() {}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__0 Point) string {
    var retv66 string
    retv66 = "Point"
    return retv66
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv68 string
    var jp70 string
    switch self__1.(type) {
    case Just:
        var x61 int32 = self__1.(Just)._0
        var value__2 int32 = x61
        var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t72 string = "Just(" + t71
        var t73 string = t72 + ")"
        jp70 = t73
    case Nothing:
        jp70 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv68 = jp70
    return retv68
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv75 Maybe__int32
    var jp77 Maybe__int32
    if flag__3 {
        var t78 Maybe__int32 = Just{
            _0: 42,
        }
        jp77 = t78
    } else {
        jp77 = Nothing{}
    }
    retv75 = jp77
    return retv75
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t80 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t80)
    var t81 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t81)
    var t82 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t82)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__5)
    retv84 = t85
    return retv84
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv90 string
    retv90 = self__37
    return retv90
}

func main() {
    main0()
}
