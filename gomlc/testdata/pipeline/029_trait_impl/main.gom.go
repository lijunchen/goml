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
    return "Point"
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    switch self__1.(type) {
    case Just:
        var x155 int32 = self__1.(Just)._0
        var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x155)
        var t166 string = "Just(" + t165
        var t167 string = t166 + ")"
        return t167
    case Nothing:
        return "Nothing"
    default:
        panic("non-exhaustive match")
    }
}

func make_maybe(flag__3 bool) Maybe__int32 {
    if flag__3 {
        var t172 Maybe__int32 = Just{
            _0: 42,
        }
        return t172
    } else {
        return Nothing{}
    }
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t174 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t174)
    var t175 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t175)
    var t176 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t176)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t179 string = _goml_runtime_core_int32_to_string(self__6)
    return t179
}

func println__T_string(value__1 string) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
