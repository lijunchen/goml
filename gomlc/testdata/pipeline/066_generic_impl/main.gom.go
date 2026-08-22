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

type Point__i32__string struct {
    x int32
    y string
}

type Point__string__string struct {
    x string
    y string
}

type Point__string__i32 struct {
    x string
    y int32
}

type Ordering int32

func main0() struct{} {
    var p1__4 Point__i32__string
    var inline456 int32 = 10
    var inline457 string = "hello"
    var inline458 Point__i32__string = Point__i32__string{
        x: inline456,
        y: inline457,
    }
    p1__4 = inline458
    var p2__5 Point__string__string
    var inline452 string = "goml"
    var inline453 string = "lang"
    var inline454 Point__string__string = Point__string__string{
        x: inline452,
        y: inline453,
    }
    p2__5 = inline454
    var p3__6 Point__string__i32
    var inline448 string = p1__4.y
    var inline449 int32 = p1__4.x
    var inline450 Point__string__i32 = Point__string__i32{
        x: inline448,
        y: inline449,
    }
    p3__6 = inline450
    var x__7 int32 = p3__6.y
    var t413 string
    var inline446 string = _goml_runtime_core_int32_to_string(x__7)
    t413 = inline446
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t413)
    _goml_runtime_core_string_println(inline443)
    var x2__8 string
    var inline441 string = p2__5.x
    x2__8 = inline441
    var inline438 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline438)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
