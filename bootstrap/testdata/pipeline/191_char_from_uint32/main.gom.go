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
    var mtmp64 Option__char = char_from_uint32(value__0)
    switch mtmp64.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x65 rune = mtmp64.(Some)._0
        var result__1 rune = x65
        var t79 uint32 = uint32(rune(result__1))
        println__T_uint32(t79)
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
    var mtmp74 Option__char = char_from_uint32(128512)
    switch mtmp74.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x75 rune = mtmp74.(Some)._0
        var result__2 rune = x75
        var t84 string = _goml_m_inherent_i_char_i_char_i_to__string(result__2)
        println__T_string(t84)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv87 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp89 Option__char
    if valid__3 {
        var t90 Option__char = Some{
            _0: value__4,
        }
        jp89 = t90
    } else {
        jp89 = None{}
    }
    retv87 = jp89
    return retv87
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv98 string
    var t99 string = _goml_runtime_core_char_to_string(self__7)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv101 string
    retv101 = self__38
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv103 string
    var t104 string = _goml_runtime_core_uint32_to_string(self__47)
    retv103 = t104
    return retv103
}

func main() {
    main0()
}
