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
    var retv12 string
    retv12 = "Point"
    return retv12
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv14 string
    var jp16 string
    switch self__1.(type) {
    case Just:
        var x7 int32 = self__1.(Just)._0
        var value__2 int32 = x7
        var t17 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t18 string = "Just(" + t17
        var t19 string = t18 + ")"
        jp16 = t19
    case Nothing:
        jp16 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv14 = jp16
    return retv14
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv21 Maybe__int32
    var jp23 Maybe__int32
    if flag__3 {
        var t24 Maybe__int32 = Just{
            _0: 42,
        }
        jp23 = t24
    } else {
        jp23 = Nothing{}
    }
    retv21 = jp23
    return retv21
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t26 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t26)
    var t27 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t27)
    var t28 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t28)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv30 string
    var t31 string = _goml_runtime_core_int32_to_string(self__2)
    retv30 = t31
    return retv30
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv36 string
    retv36 = self__9
    return retv36
}

func main() {
    main0()
}
