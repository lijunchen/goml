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
    var inline227 int32 = 10
    var inline228 string = "hello"
    var inline229 Point__int32__string = Point__int32__string{
        x: inline227,
        y: inline228,
    }
    p1__4 = inline229
    var p2__5 Point__string__string
    var inline223 string = "goml"
    var inline224 string = "lang"
    var inline225 Point__string__string = Point__string__string{
        x: inline223,
        y: inline224,
    }
    p2__5 = inline225
    var p3__6 Point__string__int32
    var inline219 string = p1__4.y
    var inline220 int32 = p1__4.x
    var inline221 Point__string__int32 = Point__string__int32{
        x: inline219,
        y: inline220,
    }
    p3__6 = inline221
    var x__7 int32 = p3__6.y
    var t184 string
    var inline217 string = _goml_runtime_core_int32_to_string(x__7)
    t184 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline214)
    var x2__8 string
    var inline212 string = p2__5.x
    x2__8 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
