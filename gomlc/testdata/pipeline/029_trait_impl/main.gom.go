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
    var retv160 string
    retv160 = "Point"
    return retv160
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv162 string
    var jp164 string
    switch self__1.(type) {
    case Just:
        var x155 int32 = self__1.(Just)._0
        var value__2 int32 = x155
        var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t166 string = "Just(" + t165
        var t167 string = t166 + ")"
        jp164 = t167
    case Nothing:
        jp164 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv162 = jp164
    return retv162
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv169 Maybe__int32
    var jp171 Maybe__int32
    if flag__3 {
        var t172 Maybe__int32 = Just{
            _0: 42,
        }
        jp171 = t172
    } else {
        jp171 = Nothing{}
    }
    retv169 = jp171
    return retv169
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
    var retv178 string
    var t179 string = _goml_runtime_core_int32_to_string(self__6)
    retv178 = t179
    return retv178
}

func println__T_string(value__1 string) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv184 string
    retv184 = self__38
    return retv184
}

func main() {
    main0()
}
