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
    var inline200 int32 = 10
    var inline201 string = "hello"
    var inline202 Point__int32__string = Point__int32__string{
        x: inline200,
        y: inline201,
    }
    p1__4 = inline202
    var p2__5 Point__string__string
    var inline196 string = "goml"
    var inline197 string = "lang"
    var inline198 Point__string__string = Point__string__string{
        x: inline196,
        y: inline197,
    }
    p2__5 = inline198
    var p3__6 Point__string__int32
    var inline192 string = p1__4.y
    var inline193 int32 = p1__4.x
    var inline194 Point__string__int32 = Point__string__int32{
        x: inline192,
        y: inline193,
    }
    p3__6 = inline194
    var x__7 int32 = p3__6.y
    var t157 string
    var inline190 string = _goml_runtime_core_int32_to_string(x__7)
    t157 = inline190
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline187)
    var x2__8 string
    var inline185 string = p2__5.x
    x2__8 = inline185
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline182)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
