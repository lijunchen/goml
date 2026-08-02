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
    var commute_field255 rune
    var inline204 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
    var inline205 bool = inline204._0
    var inline206 rune = inline204._1
    if inline205 {
        commute_field255 = inline206
        var t170 uint32 = uint32(rune(commute_field255))
        var inline201 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t170)
        _goml_runtime_core_string_println(inline201)
        return struct{}{}
    } else {
        var inline197 string = "none"
        var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline197)
        _goml_runtime_core_string_println(inline198)
        return struct{}{}
    }
}

func main0() struct{} {
    show_scalar(0)
    show_scalar(65)
    show_scalar(55295)
    show_scalar(55296)
    show_scalar(57343)
    var inline244 uint32 = 57344
    var inline245 Option__char = char_from_uint32(inline244)
    switch inline245.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline247 rune = inline245.(Some)._0
        var inline249 uint32 = uint32(rune(inline247))
        println__T_uint32(inline249)
    default:
        panic("non-exhaustive match")
    }
    var inline236 uint32 = 1114111
    var inline237 Option__char = char_from_uint32(inline236)
    switch inline237.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline239 rune = inline237.(Some)._0
        var inline241 uint32 = uint32(rune(inline239))
        println__T_uint32(inline241)
    default:
        panic("non-exhaustive match")
    }
    var inline228 uint32 = 1114112
    var inline229 Option__char = char_from_uint32(inline228)
    switch inline229.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline231 rune = inline229.(Some)._0
        var inline233 uint32 = uint32(rune(inline231))
        println__T_uint32(inline233)
    default:
        panic("non-exhaustive match")
    }
    var commute_field258 rune
    var inline220 uint32 = 128512
    var inline221 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline220)
    var inline222 bool = inline221._0
    var inline223 rune = inline221._1
    if inline222 {
        commute_field258 = inline223
        var t175 string
        var inline218 string = _goml_runtime_core_char_to_string(commute_field258)
        t175 = inline218
        var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t175)
        _goml_runtime_core_string_println(inline215)
        return struct{}{}
    } else {
        var inline211 string = "none"
        var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline211)
        _goml_runtime_core_string_println(inline212)
        return struct{}{}
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    if x1 {
        var t181 Option__char = Some{
            _0: x2,
        }
        return t181
    } else {
        return None{}
    }
}

func println__T_string(value__1 string) struct{} {
    var t183 string
    t183 = value__1
    _goml_runtime_core_string_println(t183)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t186 string
    var inline253 string = _goml_runtime_core_uint32_to_string(value__1)
    t186 = inline253
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var t195 string = _goml_runtime_core_uint32_to_string(self__47)
    return t195
}

func main() {
    main0()
}
