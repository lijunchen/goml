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
    var inline232 int32 = 10
    var inline233 string = "hello"
    var inline234 Point__int32__string = Point__int32__string{
        x: inline232,
        y: inline233,
    }
    p1__4 = inline234
    var p2__5 Point__string__string
    var inline228 string = "goml"
    var inline229 string = "lang"
    var inline230 Point__string__string = Point__string__string{
        x: inline228,
        y: inline229,
    }
    p2__5 = inline230
    var p3__6 Point__string__int32
    var inline224 string = p1__4.y
    var inline225 int32 = p1__4.x
    var inline226 Point__string__int32 = Point__string__int32{
        x: inline224,
        y: inline225,
    }
    p3__6 = inline226
    var x__7 int32 = p3__6.y
    var t189 string
    var inline222 string = _goml_runtime_core_int32_to_string(x__7)
    t189 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline219)
    var x2__8 string
    var inline217 string = p2__5.x
    x2__8 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
