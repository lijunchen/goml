package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

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

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
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

func show_u8(value__0 uint8) struct{} {
    var inline679 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline679)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t496_lhs uint8 = 13
    var t496_rhs uint8 = 5
    var t496 uint8 = t496_lhs % t496_rhs
    show_u8(t496)
    var t497_lhs uint8 = 12
    var t497_rhs uint8 = 10
    var t497 uint8 = t497_lhs & t497_rhs
    show_u8(t497)
    var t498_lhs uint8 = 12
    var t498_rhs uint8 = 3
    var t498 uint8 = t498_lhs | t498_rhs
    show_u8(t498)
    var t499_lhs uint8 = 12
    var t499_rhs uint8 = 10
    var t499 uint8 = t499_lhs ^ t499_rhs
    show_u8(t499)
    var t500_lhs uint8 = 1
    var t500_rhs int = 7
    var t500 uint8 = t500_lhs << t500_rhs
    show_u8(t500)
    var t501_lhs uint8 = 128
    var t501_rhs int = 7
    var t501 uint8 = t501_lhs >> t501_rhs
    println__T_uint8(t501)
    var t502_operand uint8 = 0
    var t502 uint8 = ^t502_operand
    println__T_uint8(t502)
    var t503_lhs uint8 = 1
    var t503_rhs int = 8
    var t503 uint8 = t503_lhs << t503_rhs
    println__T_uint8(t503)
    var t504_lhs uint16 = 513
    var t504_rhs uint16 = 256
    var t504 uint16 = t504_lhs % t504_rhs
    println__T_uint16(t504)
    var t505_lhs uint16 = 3855
    var t505_rhs uint16 = 255
    var t505 uint16 = t505_lhs & t505_rhs
    println__T_uint16(t505)
    var t506_lhs uint16 = 3840
    var t506_rhs uint16 = 15
    var t506 uint16 = t506_lhs | t506_rhs
    println__T_uint16(t506)
    var t507_lhs uint16 = 43690
    var t507_rhs uint16 = 3855
    var t507 uint16 = t507_lhs ^ t507_rhs
    println__T_uint16(t507)
    var t508_lhs uint16 = 1
    var t508_rhs int = 15
    var t508 uint16 = t508_lhs << t508_rhs
    println__T_uint16(t508)
    var t509_lhs uint16 = 32768
    var t509_rhs int = 15
    var t509 uint16 = t509_lhs >> t509_rhs
    println__T_uint16(t509)
    var t510_operand uint16 = 0
    var t510 uint16 = ^t510_operand
    println__T_uint16(t510)
    var t511_lhs uint32 = 1000000001
    var t511_rhs uint32 = 1000
    var t511 uint32 = t511_lhs % t511_rhs
    println__T_uint32(t511)
    var t512_lhs uint32 = 4042322160
    var t512_rhs uint32 = 252645135
    var t512 uint32 = t512_lhs & t512_rhs
    println__T_uint32(t512)
    var t513_lhs uint32 = 4042322160
    var t513_rhs uint32 = 252645135
    var t513 uint32 = t513_lhs | t513_rhs
    println__T_uint32(t513)
    var t514_lhs uint32 = 4042322160
    var t514_rhs uint32 = 252645135
    var t514 uint32 = t514_lhs ^ t514_rhs
    println__T_uint32(t514)
    var t515_lhs uint32 = 1
    var t515_rhs int = 31
    var t515 uint32 = t515_lhs << t515_rhs
    println__T_uint32(t515)
    var t516_lhs uint32 = 2147483648
    var t516_rhs int = 31
    var t516 uint32 = t516_lhs >> t516_rhs
    println__T_uint32(t516)
    var t517_operand uint32 = 0
    var t517 uint32 = ^t517_operand
    println__T_uint32(t517)
    var t518_lhs uint64 = 1000000000001
    var t518_rhs uint64 = 1000
    var t518 uint64 = t518_lhs % t518_rhs
    println__T_uint64(t518)
    var t519_lhs uint64 = 17361641481138401520
    var t519_rhs uint64 = 1085102592571150095
    var t519 uint64 = t519_lhs & t519_rhs
    println__T_uint64(t519)
    var t520_lhs uint64 = 17361641481138401520
    var t520_rhs uint64 = 1085102592571150095
    var t520 uint64 = t520_lhs | t520_rhs
    println__T_uint64(t520)
    var t521_lhs uint64 = 17361641481138401520
    var t521_rhs uint64 = 1085102592571150095
    var t521 uint64 = t521_lhs ^ t521_rhs
    println__T_uint64(t521)
    var t522_lhs uint64 = 1
    var t522_rhs int = 63
    var t522 uint64 = t522_lhs << t522_rhs
    println__T_uint64(t522)
    var t523_lhs uint64 = 9223372036854775808
    var t523_rhs int = 63
    var t523 uint64 = t523_lhs >> t523_rhs
    println__T_uint64(t523)
    var t524_operand uint64 = 0
    var t524 uint64 = ^t524_operand
    println__T_uint64(t524)
    return struct{}{}
}

func signed_ops() struct{} {
    var t527_lhs int8 = -13
    var t527_rhs int8 = 5
    var t527 int8 = t527_lhs % t527_rhs
    println__T_int8(t527)
    var t528_lhs int8 = -8
    var t528_rhs int = 2
    var t528 int8 = t528_lhs >> t528_rhs
    println__T_int8(t528)
    var t529_lhs int8 = 1
    var t529_rhs int = 6
    var t529 int8 = t529_lhs << t529_rhs
    println__T_int8(t529)
    var t530_operand int8 = 0
    var t530 int8 = ^t530_operand
    println__T_int8(t530)
    var t531_lhs int8 = -1
    var t531_rhs int = 7
    var t531 int8 = t531_lhs >> t531_rhs
    println__T_int8(t531)
    var t532_lhs int16 = -513
    var t532_rhs int16 = 256
    var t532 int16 = t532_lhs % t532_rhs
    println__T_int16(t532)
    var t533 int16 = -32767 - 1
    var t534_rhs int = 15
    var t534 int16 = t533 >> t534_rhs
    println__T_int16(t534)
    var t535_lhs int16 = 1
    var t535_rhs int = 14
    var t535 int16 = t535_lhs << t535_rhs
    println__T_int16(t535)
    var t536_operand int16 = 255
    var t536 int16 = ^t536_operand
    println__T_int16(t536)
    var t537_lhs int32 = -1000000001
    var t537_rhs int32 = 1000
    var t537 int32 = t537_lhs % t537_rhs
    println__T_int32(t537)
    var t538 int32 = -2147483647 - 1
    var t539_rhs int = 31
    var t539 int32 = t538 >> t539_rhs
    println__T_int32(t539)
    var t540_lhs int32 = 1
    var t540_rhs int = 30
    var t540 int32 = t540_lhs << t540_rhs
    println__T_int32(t540)
    var t541_operand int32 = 65535
    var t541 int32 = ^t541_operand
    println__T_int32(t541)
    var t542_lhs int64 = -1000000000001
    var t542_rhs int64 = 1000
    var t542 int64 = t542_lhs % t542_rhs
    println__T_int64(t542)
    var t543_lhs int64 = -9223372036854775807
    var t543_rhs int = 62
    var t543 int64 = t543_lhs >> t543_rhs
    println__T_int64(t543)
    var t544_lhs int64 = 1
    var t544_rhs int = 62
    var t544 int64 = t544_lhs << t544_rhs
    println__T_int64(t544)
    var t545_operand int64 = 4294967295
    var t545 int64 = ^t545_operand
    println__T_int64(t545)
    return struct{}{}
}

func precedence() struct{} {
    var t548_lhs uint8 = 3
    var t548_rhs uint8 = 1
    var t548 uint8 = t548_lhs & t548_rhs
    var t549_lhs uint8 = 2
    var t549 uint8 = t549_lhs ^ t548
    var t550_lhs uint8 = 1
    var t550 uint8 = t550_lhs | t549
    println__T_uint8(t550)
    var t551 int = 2 + 1
    var t552_lhs uint8 = 1
    var t552 uint8 = t552_lhs << t551
    println__T_uint8(t552)
    var t553_lhs int = 1
    var t553_rhs int = 2
    var t553 int = t553_lhs | t553_rhs
    var t554 bool = t553 == 3
    var t555 string
    var inline795 string = _goml_runtime_core_bool_to_string(t554)
    t555 = inline795
    var inline792 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t555)
    _goml_runtime_core_string_println(inline792)
    var t556_lhs int = 8
    var t556_rhs int = 1
    var t556 int = t556_lhs >> t556_rhs
    var t557 bool = t556 < 5
    var t558 string
    var inline790 string = _goml_runtime_core_bool_to_string(t557)
    t558 = inline790
    var inline787 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t558)
    _goml_runtime_core_string_println(inline787)
    var t559_operand uint8 = 1
    var t559 uint8 = ^t559_operand
    var t560_rhs uint8 = 15
    var t560 uint8 = t559 & t560_rhs
    println__T_uint8(t560)
    return struct{}{}
}

func casts() struct{} {
    var five_eleven__8 uint16 = 511
    var two_fifty_six__9 uint16 = 256
    var negative_one_i16__10 int16 = -1
    var two_fifty_five__11 uint8 = 255
    var one_twenty_eight__12 uint8 = 128
    var negative_one_twenty_nine__13 int16 = -129
    var max_u16__14 uint16 = 65535
    var negative_one_i32__15 int32 = -1
    var negative_one_i8__16 int8 = -1
    var max_u64__17 uint64 = 18446744073709551615
    var sixty_five__18 uint8 = 65
    var max_u32__19 uint32 = 4294967295
    var three_hundred__20 uint16 = 300
    var t563 uint8 = uint8(uint16(five_eleven__8))
    println__T_uint8(t563)
    var t564 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_uint8(t564)
    var t565 uint8 = uint8(int16(negative_one_i16__10))
    println__T_uint8(t565)
    var t566 int8 = int8(uint8(two_fifty_five__11))
    println__T_int8(t566)
    var t567 int8 = int8(uint8(one_twenty_eight__12))
    println__T_int8(t567)
    var t568 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_int8(t568)
    var t569 int16 = int16(uint16(max_u16__14))
    println__T_int16(t569)
    var t570 uint16 = uint16(int32(negative_one_i32__15))
    println__T_uint16(t570)
    var t571 uint64 = uint64(int8(negative_one_i8__16))
    println__T_uint64(t571)
    var t572 int32 = int32(uint64(max_u64__17))
    println__T_int32(t572)
    var t573 uint32 = uint32(uint8(sixty_five__18))
    println__T_uint32(t573)
    var t574 int64 = int64(uint32(max_u32__19))
    println__T_int64(t574)
    var t575_source rune = 65
    var t575 uint32 = uint32(rune(t575_source))
    println__T_uint32(t575)
    var mtmp472 Option__char
    var inline812 uint32 = 128512
    var inline813 Option__char = __goml_builtin_char_from_uint32(inline812)
    mtmp472 = inline813
    switch mtmp472._tag {
    case 0:
        var inline801 string = "invalid"
        var inline802 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline801)
        _goml_runtime_core_string_println(inline802)
    case 1:
        var x473 rune = mtmp472._v1_0
        var t581 string
        var inline808 string = char_to_string(x473)
        t581 = inline808
        var inline805 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t581)
        _goml_runtime_core_string_println(inline805)
    default:
        panic("non-exhaustive match")
    }
    var t577 uint8 = uint8(uint16(three_hundred__20))
    var t578 uint32 = uint32(uint8(t577))
    println__T_uint32(t578)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t591 uint8
    var inline843 uint8 = 10
    var inline844 uint8 = ^inline843
    var inline845_rhs uint8 = 15
    var inline845 uint8 = inline844 & inline845_rhs
    var inline846_lhs uint8 = 1
    var inline846_rhs int = 4
    var inline846 uint8 = inline846_lhs << inline846_rhs
    var inline847_rhs uint8 = 31
    var inline847 uint8 = inline846 % inline847_rhs
    var inline848 uint8 = inline845 | inline847
    t591 = inline848
    println__T_uint8(t591)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t594 string
    var inline850 string = _goml_runtime_core_uint8_to_string(value__1)
    t594 = inline850
    _goml_runtime_core_string_println(t594)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t597 string
    var inline852 string = _goml_runtime_core_uint16_to_string(value__1)
    t597 = inline852
    _goml_runtime_core_string_println(t597)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t600 string
    var inline854 string = _goml_runtime_core_uint32_to_string(value__1)
    t600 = inline854
    _goml_runtime_core_string_println(t600)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t603 string
    var inline856 string = _goml_runtime_core_uint64_to_string(value__1)
    t603 = inline856
    _goml_runtime_core_string_println(t603)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t606 string
    var inline858 string = _goml_runtime_core_int8_to_string(value__1)
    t606 = inline858
    _goml_runtime_core_string_println(t606)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t609 string
    var inline860 string = _goml_runtime_core_int16_to_string(value__1)
    t609 = inline860
    _goml_runtime_core_string_println(t609)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t612 string
    var inline862 string = _goml_runtime_core_int32_to_string(value__1)
    t612 = inline862
    _goml_runtime_core_string_println(t612)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t615 string
    var inline864 string = _goml_runtime_core_int64_to_string(value__1)
    t615 = inline864
    _goml_runtime_core_string_println(t615)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__156 uint8) string {
    var t631 string = _goml_runtime_core_uint8_to_string(self__156)
    return t631
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t659 bool
    var inline878 bool = value__30 <= 1114111
    if inline878 {
        var inline879 bool = value__30 >= 55296
        var inline881 bool
        if inline879 {
            var inline883 bool = value__30 <= 57343
            inline881 = inline883
        } else {
            inline881 = false
        }
        var inline882 bool = !inline881
        t659 = inline882
    } else {
        t659 = false
    }
    if t659 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t660 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t660
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func char_to_string(value__29 rune) string {
    var t665 uint32 = uint32(rune(value__29))
    var t666 bool
    var inline885 bool = t665 <= 1114111
    if inline885 {
        var inline886 bool = t665 >= 55296
        var inline888 bool
        if inline886 {
            var inline890 bool = t665 <= 57343
            inline888 = inline890
        } else {
            inline888 = false
        }
        var inline889 bool = !inline888
        t666 = inline889
    } else {
        t666 = false
    }
    if t666 {
        var t667 string = _goml_runtime_core_char_to_string(value__29)
        return t667
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
