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

type Point__int32__string struct {
    x int32
    y string
}

type Point__string__string struct {
    x string
    y string
}

type Point__string__int32 struct {
    x string
    y int32
}

func main0() struct{} {
    var p1__4 Point__int32__string
    var inline217 int32 = 10
    var inline218 string = "hello"
    var inline219 Point__int32__string = Point__int32__string{
        x: inline217,
        y: inline218,
    }
    p1__4 = inline219
    var p2__5 Point__string__string
    var inline213 string = "goml"
    var inline214 string = "lang"
    var inline215 Point__string__string = Point__string__string{
        x: inline213,
        y: inline214,
    }
    p2__5 = inline215
    var p3__6 Point__string__int32
    var inline209 string = p1__4.y
    var inline210 int32 = p1__4.x
    var inline211 Point__string__int32 = Point__string__int32{
        x: inline209,
        y: inline210,
    }
    p3__6 = inline211
    var x__7 int32 = p3__6.y
    var t174 string
    var inline207 string = _goml_runtime_core_int32_to_string(x__7)
    t174 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline204)
    var x2__8 string
    var inline202 string = p2__5.x
    x2__8 = inline202
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
