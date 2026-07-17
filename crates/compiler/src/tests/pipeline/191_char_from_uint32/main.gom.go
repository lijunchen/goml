package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    if value > 1114111 || value >= 55296 && value <= 57343 {
        return Tuple2_4bool_4char{
            _0: false,
            _1: 0,
        }
    }
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Option__char interface {
    isOption__char()
}

type None struct {}

func (_ None) isOption__char() {}

type Some struct {
    _0 rune
}

func (_ Some) isOption__char() {}

func show_scalar(value__0 uint32) struct{} {
    var mtmp61 Option__char = char_from_uint32(value__0)
    switch mtmp61.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x62 rune = mtmp61.(Some)._0
        var result__1 rune = x62
        var t76 uint32 = uint32(rune(result__1))
        println__T_uint32(t76)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    show_scalar(0)
    show_scalar(65)
    show_scalar(55295)
    show_scalar(55296)
    show_scalar(57343)
    show_scalar(57344)
    show_scalar(1114111)
    show_scalar(1114112)
    var mtmp71 Option__char = char_from_uint32(128512)
    switch mtmp71.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x72 rune = mtmp71.(Some)._0
        var result__2 rune = x72
        var t81 string = _goml_m_inherent_i_char_i_char_i_to__string(result__2)
        println__T_string(t81)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv84 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp86 Option__char
    if valid__3 {
        var t87 Option__char = Some{
            _0: value__4,
        }
        jp86 = t87
    } else {
        jp86 = None{}
    }
    retv84 = jp86
    return retv84
}

func println__T_string(value__1 string) struct{} {
    var t89 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t89)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv95 string
    var t96 string = _goml_runtime_core_char_to_string(self__6)
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv98 string
    retv98 = self__37
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__45 uint32) string {
    var retv100 string
    var t101 string = _goml_runtime_core_uint32_to_string(self__45)
    retv100 = t101
    return retv100
}

func main() {
    main0()
}
