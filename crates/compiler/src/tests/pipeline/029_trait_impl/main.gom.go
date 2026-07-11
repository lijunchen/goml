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
    var retv9 string
    retv9 = "Point"
    return retv9
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv11 string
    var jp13 string
    switch self__1.(type) {
    case Just:
        var x4 int32 = self__1.(Just)._0
        var value__2 int32 = x4
        var t14 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t15 string = "Just(" + t14
        var t16 string = t15 + ")"
        jp13 = t16
    case Nothing:
        jp13 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv11 = jp13
    return retv11
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv18 Maybe__int32
    var jp20 Maybe__int32
    if flag__3 {
        var t21 Maybe__int32 = Just{
            _0: 42,
        }
        jp20 = t21
    } else {
        jp20 = Nothing{}
    }
    retv18 = jp20
    return retv18
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t23 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t23)
    var t24 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t24)
    var t25 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t25)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv27 string
    var t28 string = _goml_runtime_core_int32_to_string(self__2)
    retv27 = t28
    return retv27
}

func println__T_string(value__1 string) struct{} {
    var t30 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t30)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv33 string
    retv33 = self__9
    return retv33
}

func main() {
    main0()
}
