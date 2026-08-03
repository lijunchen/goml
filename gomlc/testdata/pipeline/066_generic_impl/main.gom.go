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
    var inline222 int32 = 10
    var inline223 string = "hello"
    var inline224 Point__int32__string = Point__int32__string{
        x: inline222,
        y: inline223,
    }
    p1__4 = inline224
    var p2__5 Point__string__string
    var inline218 string = "goml"
    var inline219 string = "lang"
    var inline220 Point__string__string = Point__string__string{
        x: inline218,
        y: inline219,
    }
    p2__5 = inline220
    var p3__6 Point__string__int32
    var inline214 string = p1__4.y
    var inline215 int32 = p1__4.x
    var inline216 Point__string__int32 = Point__string__int32{
        x: inline214,
        y: inline215,
    }
    p3__6 = inline216
    var x__7 int32 = p3__6.y
    var t179 string
    var inline212 string = _goml_runtime_core_int32_to_string(x__7)
    t179 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
    _goml_runtime_core_string_println(inline209)
    var x2__8 string
    var inline207 string = p2__5.x
    x2__8 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
