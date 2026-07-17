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
    var retv63 string
    retv63 = "Point"
    return retv63
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv65 string
    var jp67 string
    switch self__1.(type) {
    case Just:
        var x58 int32 = self__1.(Just)._0
        var value__2 int32 = x58
        var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t69 string = "Just(" + t68
        var t70 string = t69 + ")"
        jp67 = t70
    case Nothing:
        jp67 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv65 = jp67
    return retv65
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv72 Maybe__int32
    var jp74 Maybe__int32
    if flag__3 {
        var t75 Maybe__int32 = Just{
            _0: 42,
        }
        jp74 = t75
    } else {
        jp74 = Nothing{}
    }
    retv72 = jp74
    return retv72
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t77 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t77)
    var t78 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t78)
    var t79 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t79)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__2)
    retv81 = t82
    return retv81
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv87 string
    retv87 = self__34
    return retv87
}

func main() {
    main0()
}
