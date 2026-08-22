package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint_to_string(x uint) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_float32_to_string(x float32) string {
    var formatted string = _goml_strconv.FormatFloat(float64(x), 102, -1, 32)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

func _goml_runtime_core_float64_to_string(x float64) string {
    var formatted string = _goml_strconv.FormatFloat(x, 102, -1, 64)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
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

func main0() struct{} {
    var small__10 float32 = 1.5
    var large__11 float64 = 2.25
    var value__12 uint8 = 65
    var t436 int
    var inline552 int8 = -1
    var inline553 int16 = 2
    var inline554 int32 = 3
    var inline555 int64 = 4
    var inline556 int = 5
    var inline557 int = int(int8(inline552))
    var inline558 int = int(int16(inline553))
    var inline559 int = inline557 + inline558
    var inline560 int = int(int32(inline554))
    var inline561 int = inline559 + inline560
    var inline562 int = int(int64(inline555))
    var inline563 int = inline561 + inline562
    var inline564 int = inline563 + inline556
    t436 = inline564
    var inline549 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t436)
    _goml_runtime_core_string_println(inline549)
    var t437 uint
    var inline535 uint8 = 1
    var inline536 uint16 = 2
    var inline537 uint32 = 3
    var inline538 uint64 = 4
    var inline539 uint = 5
    var inline540 uint = uint(uint8(inline535))
    var inline541 uint = uint(uint16(inline536))
    var inline542 uint = inline540 + inline541
    var inline543 uint = uint(uint32(inline537))
    var inline544 uint = inline542 + inline543
    var inline545 uint = uint(uint64(inline538))
    var inline546 uint = inline544 + inline545
    var inline547 uint = inline546 + inline539
    t437 = inline547
    var inline532 string = _goml_m_trait__impl_i_ToString_i_usize_i_to__string(t437)
    _goml_runtime_core_string_println(inline532)
    var t438 string
    var inline530 string = _goml_runtime_core_float32_to_string(small__10)
    t438 = inline530
    var t439 string = t438 + ","
    var t440 string
    var inline528 string = _goml_runtime_core_float64_to_string(large__11)
    t440 = inline528
    var t441 string = t439 + t440
    var inline525 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline525)
    var t442 uint16 = uint16(uint8(value__12))
    var inline522 string = _goml_m_trait__impl_i_ToString_i_u16_i_to__string(t442)
    _goml_runtime_core_string_println(inline522)
    var t443 Option__char
    var inline519 uint32 = 65
    var inline520 Option__char = __goml_builtin_char_from_uint32(inline519)
    t443 = inline520
    var t444 rune
    var inline515 rune = 63
    switch t443._tag {
    case 0:
        t444 = inline515
    case 1:
        var inline516 rune = t443._v1_0
        t444 = inline516
    default:
        panic("non-exhaustive match")
    }
    var inline512 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(t444)
    _goml_runtime_core_string_println(inline512)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t476 string = _goml_runtime_core_int_to_string(self__151)
    return t476
}

func _goml_m_trait__impl_i_ToString_i_usize_i_to__string(self__451 uint) string {
    var t479 string = _goml_runtime_core_uint_to_string(self__451)
    return t479
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_u16_i_to__string(self__157 uint16) string {
    var t484 string = _goml_runtime_core_uint16_to_string(self__157)
    return t484
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__150 rune) string {
    var inline581 uint32 = uint32(rune(self__150))
    var inline582 bool = utf8_valid_scalar(inline581)
    if inline582 {
        var inline583 string = _goml_runtime_core_char_to_string(self__150)
        return inline583
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t492 bool
    var inline586 bool = value__30 <= 1114111
    if inline586 {
        var inline587 bool = value__30 >= 55296
        var inline589 bool
        if inline587 {
            var inline591 bool = value__30 <= 57343
            inline589 = inline591
        } else {
            inline589 = false
        }
        var inline590 bool = !inline589
        t492 = inline590
    } else {
        t492 = false
    }
    if t492 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t493 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t493
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t505 bool = value__4 <= 1114111
    if t505 {
        var t509 bool = value__4 >= 55296
        var jp507 bool
        if t509 {
            var t510 bool = value__4 <= 57343
            jp507 = t510
        } else {
            jp507 = false
        }
        var t508 bool = !jp507
        return t508
    } else {
        return false
    }
}

func main() {
    main0()
}
