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
    var retv113 string
    retv113 = "Point"
    return retv113
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv115 string
    var jp117 string
    switch self__1.(type) {
    case Just:
        var x108 int32 = self__1.(Just)._0
        var value__2 int32 = x108
        var t118 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t119 string = "Just(" + t118
        var t120 string = t119 + ")"
        jp117 = t120
    case Nothing:
        jp117 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv115 = jp117
    return retv115
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv122 Maybe__int32
    var jp124 Maybe__int32
    if flag__3 {
        var t125 Maybe__int32 = Just{
            _0: 42,
        }
        jp124 = t125
    } else {
        jp124 = Nothing{}
    }
    retv122 = jp124
    return retv122
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t127 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t127)
    var t128 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t128)
    var t129 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t129)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv131 string
    var t132 string = _goml_runtime_core_int32_to_string(self__6)
    retv131 = t132
    return retv131
}

func println__T_string(value__1 string) struct{} {
    var t134 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t134)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv137 string
    retv137 = self__38
    return retv137
}

func main() {
    main0()
}
