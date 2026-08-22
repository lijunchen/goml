package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
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

type Ordering int32

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func show_scalar(value__0 uint32) struct{} {
    var mtmp411 Option__char
    var inline481 Option__char = __goml_builtin_char_from_uint32(value__0)
    mtmp411 = inline481
    switch mtmp411._tag {
    case 0:
        var inline474 string = "none"
        var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline474)
        _goml_runtime_core_string_println(inline475)
        return struct{}{}
    case 1:
        var x412 rune = mtmp411._v1_0
        var t426 uint32 = uint32(rune(x412))
        var inline478 string = _goml_m_trait__impl_i_ToString_i_u32_i_to__string(t426)
        _goml_runtime_core_string_println(inline478)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    show_scalar(0)
    show_scalar(65)
    show_scalar(55295)
    show_scalar(55296)
    var inline519 uint32 = 57343
    var inline520 Option__char = char_from_u32(inline519)
    switch inline520._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline522 rune = inline520._v1_0
        var inline524 uint32 = uint32(rune(inline522))
        println__T_u32(inline524)
    default:
        panic("non-exhaustive match")
    }
    var inline511 uint32 = 57344
    var inline512 Option__char = char_from_u32(inline511)
    switch inline512._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline514 rune = inline512._v1_0
        var inline516 uint32 = uint32(rune(inline514))
        println__T_u32(inline516)
    default:
        panic("non-exhaustive match")
    }
    var inline503 uint32 = 1114111
    var inline504 Option__char = char_from_u32(inline503)
    switch inline504._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline506 rune = inline504._v1_0
        var inline508 uint32 = uint32(rune(inline506))
        println__T_u32(inline508)
    default:
        panic("non-exhaustive match")
    }
    var inline495 uint32 = 1114112
    var inline496 Option__char = char_from_u32(inline495)
    switch inline496._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline498 rune = inline496._v1_0
        var inline500 uint32 = uint32(rune(inline498))
        println__T_u32(inline500)
    default:
        panic("non-exhaustive match")
    }
    var mtmp421 Option__char
    var inline492 uint32 = 128512
    var inline493 Option__char = __goml_builtin_char_from_uint32(inline492)
    mtmp421 = inline493
    switch mtmp421._tag {
    case 0:
        var inline483 string = "none"
        var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline483)
        _goml_runtime_core_string_println(inline484)
        return struct{}{}
    case 1:
        var x422 rune = mtmp421._v1_0
        var t431 string
        var inline490 string = char_to_string(x422)
        t431 = inline490
        var inline487 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
        _goml_runtime_core_string_println(inline487)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func char_from_u32(value__2 uint32) Option__char {
    var inline527 bool = utf8_valid_scalar(value__2)
    if inline527 {
        var inline528 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline529 rune = inline528._1
        var inline531 Option__char = Option__char{
            _tag: 1,
            _v1_0: inline529,
        }
        return inline531
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t437 string
    t437 = value__1
    _goml_runtime_core_string_println(t437)
    return struct{}{}
}

func println__T_u32(value__1 uint32) struct{} {
    var t440 string
    var inline534 string = _goml_runtime_core_uint32_to_string(value__1)
    t440 = inline534
    _goml_runtime_core_string_println(t440)
    return struct{}{}
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t449 bool
    var inline541 bool = value__30 <= 1114111
    if inline541 {
        var inline542 bool = value__30 >= 55296
        var inline544 bool
        if inline542 {
            var inline546 bool = value__30 <= 57343
            inline544 = inline546
        } else {
            inline544 = false
        }
        var inline545 bool = !inline544
        t449 = inline545
    } else {
        t449 = false
    }
    if t449 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t450 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t450
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_u32_i_to__string(self__158 uint32) string {
    var t455 string = _goml_runtime_core_uint32_to_string(self__158)
    return t455
}

func char_to_string(value__29 rune) string {
    var t460 uint32 = uint32(rune(value__29))
    var t461 bool
    var inline548 bool = t460 <= 1114111
    if inline548 {
        var inline549 bool = t460 >= 55296
        var inline551 bool
        if inline549 {
            var inline553 bool = t460 <= 57343
            inline551 = inline553
        } else {
            inline551 = false
        }
        var inline552 bool = !inline551
        t461 = inline552
    } else {
        t461 = false
    }
    if t461 {
        var t462 string = _goml_runtime_core_char_to_string(value__29)
        return t462
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t467 bool = value__4 <= 1114111
    if t467 {
        var t471 bool = value__4 >= 55296
        var jp469 bool
        if t471 {
            var t472 bool = value__4 <= 57343
            jp469 = t472
        } else {
            jp469 = false
        }
        var t470 bool = !jp469
        return t470
    } else {
        return false
    }
}

func main() {
    main0()
}
