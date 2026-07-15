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
    var retv27 string
    retv27 = "Point"
    return retv27
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv29 string
    var jp31 string
    switch self__1.(type) {
    case Just:
        var x22 int32 = self__1.(Just)._0
        var value__2 int32 = x22
        var t32 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t33 string = "Just(" + t32
        var t34 string = t33 + ")"
        jp31 = t34
    case Nothing:
        jp31 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv29 = jp31
    return retv29
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv36 Maybe__int32
    var jp38 Maybe__int32
    if flag__3 {
        var t39 Maybe__int32 = Just{
            _0: 42,
        }
        jp38 = t39
    } else {
        jp38 = Nothing{}
    }
    retv36 = jp38
    return retv36
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t41 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t41)
    var t42 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t42)
    var t43 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t43)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv45 string
    var t46 string = _goml_runtime_core_int32_to_string(self__2)
    retv45 = t46
    return retv45
}

func println__T_string(value__1 string) struct{} {
    var t48 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t48)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv51 string
    retv51 = self__9
    return retv51
}

func main() {
    main0()
}
