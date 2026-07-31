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
    var retv157 string
    retv157 = "Point"
    return retv157
}

func _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(self__1 Maybe__int32) string {
    var retv159 string
    var jp161 string
    switch self__1.(type) {
    case Just:
        var x152 int32 = self__1.(Just)._0
        var value__2 int32 = x152
        var t162 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__2)
        var t163 string = "Just(" + t162
        var t164 string = t163 + ")"
        jp161 = t164
    case Nothing:
        jp161 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv159 = jp161
    return retv159
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv166 Maybe__int32
    var jp168 Maybe__int32
    if flag__3 {
        var t169 Maybe__int32 = Just{
            _0: 42,
        }
        jp168 = t169
    } else {
        jp168 = Nothing{}
    }
    retv166 = jp168
    return retv166
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t171 string = _goml_m_trait__impl_i_Display_i_Point_i_show(point__4)
    println__T_string(t171)
    var t172 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(some_number__5)
    println__T_string(t172)
    var t173 string = _goml_m_trait__impl_i_Display_i_Maybe____int32_i_show(none_number__6)
    println__T_string(t173)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv175 string
    var t176 string = _goml_runtime_core_int32_to_string(self__6)
    retv175 = t176
    return retv175
}

func println__T_string(value__1 string) struct{} {
    var t178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t178)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv181 string
    retv181 = self__38
    return retv181
}

func main() {
    main0()
}
