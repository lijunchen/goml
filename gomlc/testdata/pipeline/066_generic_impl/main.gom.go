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

type Ordering int32

func main0() struct{} {
    var p1__4 Point__int32__string
    var inline453 int32 = 10
    var inline454 string = "hello"
    var inline455 Point__int32__string = Point__int32__string{
        x: inline453,
        y: inline454,
    }
    p1__4 = inline455
    var p2__5 Point__string__string
    var inline449 string = "goml"
    var inline450 string = "lang"
    var inline451 Point__string__string = Point__string__string{
        x: inline449,
        y: inline450,
    }
    p2__5 = inline451
    var p3__6 Point__string__int32
    var inline445 string = p1__4.y
    var inline446 int32 = p1__4.x
    var inline447 Point__string__int32 = Point__string__int32{
        x: inline445,
        y: inline446,
    }
    p3__6 = inline447
    var x__7 int32 = p3__6.y
    var t410 string
    var inline443 string = _goml_runtime_core_int32_to_string(x__7)
    t410 = inline443
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t410)
    _goml_runtime_core_string_println(inline440)
    var x2__8 string
    var inline438 string = p2__5.x
    x2__8 = inline438
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x2__8)
    _goml_runtime_core_string_println(inline435)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
