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
    var inline181 int32 = 10
    var inline182 string = "hello"
    var inline183 Point__int32__string = Point__int32__string{
        x: inline181,
        y: inline182,
    }
    p1__4 = inline183
    var p2__5 Point__string__string
    var inline177 string = "goml"
    var inline178 string = "lang"
    var inline179 Point__string__string = Point__string__string{
        x: inline177,
        y: inline178,
    }
    p2__5 = inline179
    var p3__6 Point__string__int32
    var inline173 string = p1__4.y
    var inline174 int32 = p1__4.x
    var inline175 Point__string__int32 = Point__string__int32{
        x: inline173,
        y: inline174,
    }
    p3__6 = inline175
    var x__7 int32 = p3__6.y
    var t138 string
    var inline171 string = _goml_runtime_core_int32_to_string(x__7)
    t138 = inline171
    var inline168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t138)
    _goml_runtime_core_string_println(inline168)
    var x2__8 string
    var inline166 string = p2__5.x
    x2__8 = inline166
    var inline163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline163)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
