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
    var mtmp408 Option__char
    var inline478 Option__char = __goml_builtin_char_from_uint32(value__0)
    mtmp408 = inline478
    switch mtmp408.(type) {
    case None:
        var inline471 string = "none"
        var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline471)
        _goml_runtime_core_string_println(inline472)
        return struct{}{}
    case Some:
        var x409 rune = mtmp408.(Some)._0
        var t423 uint32 = uint32(rune(x409))
        var inline475 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t423)
        _goml_runtime_core_string_println(inline475)
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
    var inline516 uint32 = 57343
    var inline517 Option__char = char_from_uint32(inline516)
    switch inline517.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline519 rune = inline517.(Some)._0
        var inline521 uint32 = uint32(rune(inline519))
        println__T_uint32(inline521)
    default:
        panic("non-exhaustive match")
    }
    var inline508 uint32 = 57344
    var inline509 Option__char = char_from_uint32(inline508)
    switch inline509.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline511 rune = inline509.(Some)._0
        var inline513 uint32 = uint32(rune(inline511))
        println__T_uint32(inline513)
    default:
        panic("non-exhaustive match")
    }
    var inline500 uint32 = 1114111
    var inline501 Option__char = char_from_uint32(inline500)
    switch inline501.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline503 rune = inline501.(Some)._0
        var inline505 uint32 = uint32(rune(inline503))
        println__T_uint32(inline505)
    default:
        panic("non-exhaustive match")
    }
    var inline492 uint32 = 1114112
    var inline493 Option__char = char_from_uint32(inline492)
    switch inline493.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline495 rune = inline493.(Some)._0
        var inline497 uint32 = uint32(rune(inline495))
        println__T_uint32(inline497)
    default:
        panic("non-exhaustive match")
    }
    var mtmp418 Option__char
    var inline489 uint32 = 128512
    var inline490 Option__char = __goml_builtin_char_from_uint32(inline489)
    mtmp418 = inline490
    switch mtmp418.(type) {
    case None:
        var inline480 string = "none"
        var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline480)
        _goml_runtime_core_string_println(inline481)
        return struct{}{}
    case Some:
        var x419 rune = mtmp418.(Some)._0
        var t428 string
        var inline487 string = char_to_string(x419)
        t428 = inline487
        var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
        _goml_runtime_core_string_println(inline484)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline524 bool = utf8_valid_scalar(value__2)
    if inline524 {
        var inline525 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline526 rune = inline525._1
        var inline528 Option__char = Some{
            _0: inline526,
        }
        return inline528
    } else {
        return None{}
    }
}

func println__T_string(value__1 string) struct{} {
    var t434 string
    t434 = value__1
    _goml_runtime_core_string_println(t434)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t437 string
    var inline531 string = _goml_runtime_core_uint32_to_string(value__1)
    t437 = inline531
    _goml_runtime_core_string_println(t437)
    return struct{}{}
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t446 bool
    var inline538 bool = value__30 <= 1114111
    if inline538 {
        var inline539 bool = value__30 >= 55296
        var inline541 bool
        if inline539 {
            var inline543 bool = value__30 <= 57343
            inline541 = inline543
        } else {
            inline541 = false
        }
        var inline542 bool = !inline541
        t446 = inline542
    } else {
        t446 = false
    }
    if t446 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t447 Option__char = Some{
            _0: x24,
        }
        return t447
    } else {
        return None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__158 uint32) string {
    var t452 string = _goml_runtime_core_uint32_to_string(self__158)
    return t452
}

func char_to_string(value__29 rune) string {
    var t457 uint32 = uint32(rune(value__29))
    var t458 bool
    var inline545 bool = t457 <= 1114111
    if inline545 {
        var inline546 bool = t457 >= 55296
        var inline548 bool
        if inline546 {
            var inline550 bool = t457 <= 57343
            inline548 = inline550
        } else {
            inline548 = false
        }
        var inline549 bool = !inline548
        t458 = inline549
    } else {
        t458 = false
    }
    if t458 {
        var t459 string = _goml_runtime_core_char_to_string(value__29)
        return t459
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t464 bool = value__4 <= 1114111
    if t464 {
        var t468 bool = value__4 >= 55296
        var jp466 bool
        if t468 {
            var t469 bool = value__4 <= 57343
            jp466 = t469
        } else {
            jp466 = false
        }
        var t467 bool = !jp466
        return t467
    } else {
        return false
    }
}

func main() {
    main0()
}
