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
    var retv73 string
    retv73 = "Point"
    return retv73
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv75 string
    var jp77 string
    switch self__1.(type) {
    case Just:
        var x68 int32 = self__1.(Just)._0
        var value__2 int32 = x68
        var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t79 string = "Just(" + t78
        var t80 string = t79 + ")"
        jp77 = t80
    case Nothing:
        jp77 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv75 = jp77
    return retv75
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv82 Maybe__int32
    var jp84 Maybe__int32
    if flag__3 {
        var t85 Maybe__int32 = Just{
            _0: 42,
        }
        jp84 = t85
    } else {
        jp84 = Nothing{}
    }
    retv82 = jp84
    return retv82
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t87 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t87)
    var t88 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t88)
    var t89 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t89)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__6)
    retv91 = t92
    return retv91
}

func println__T_string(value__1 string) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv97 string
    retv97 = self__38
    return retv97
}

func main() {
    main0()
}
