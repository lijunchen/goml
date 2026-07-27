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
    var retv69 string
    retv69 = "Point"
    return retv69
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv71 string
    var jp73 string
    switch self__1.(type) {
    case Just:
        var x64 int32 = self__1.(Just)._0
        var value__2 int32 = x64
        var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t75 string = "Just(" + t74
        var t76 string = t75 + ")"
        jp73 = t76
    case Nothing:
        jp73 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv71 = jp73
    return retv71
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv78 Maybe__int32
    var jp80 Maybe__int32
    if flag__3 {
        var t81 Maybe__int32 = Just{
            _0: 42,
        }
        jp80 = t81
    } else {
        jp80 = Nothing{}
    }
    retv78 = jp80
    return retv78
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t83 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t83)
    var t84 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t84)
    var t85 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t85)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__6)
    retv87 = t88
    return retv87
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv93 string
    retv93 = self__38
    return retv93
}

func main() {
    main0()
}
