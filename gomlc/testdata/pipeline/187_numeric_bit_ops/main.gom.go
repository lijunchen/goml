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

type Option__char interface {
    isOption__char()
}

type None struct {}

func (_ None) isOption__char() {}

type Some struct {
    _0 rune
}

func (_ Some) isOption__char() {}

func show_u8(value__0 uint8) struct{} {
    var inline676 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__0)
    _goml_runtime_core_string_println(inline676)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t493_lhs uint8 = 13
    var t493_rhs uint8 = 5
    var t493 uint8 = t493_lhs % t493_rhs
    show_u8(t493)
    var t494_lhs uint8 = 12
    var t494_rhs uint8 = 10
    var t494 uint8 = t494_lhs & t494_rhs
    show_u8(t494)
    var t495_lhs uint8 = 12
    var t495_rhs uint8 = 3
    var t495 uint8 = t495_lhs | t495_rhs
    show_u8(t495)
    var t496_lhs uint8 = 12
    var t496_rhs uint8 = 10
    var t496 uint8 = t496_lhs ^ t496_rhs
    show_u8(t496)
    var t497_lhs uint8 = 1
    var t497_rhs int = 7
    var t497 uint8 = t497_lhs << t497_rhs
    show_u8(t497)
    var t498_lhs uint8 = 128
    var t498_rhs int = 7
    var t498 uint8 = t498_lhs >> t498_rhs
    println__T_uint8(t498)
    var t499_operand uint8 = 0
    var t499 uint8 = ^t499_operand
    println__T_uint8(t499)
    var t500_lhs uint8 = 1
    var t500_rhs int = 8
    var t500 uint8 = t500_lhs << t500_rhs
    println__T_uint8(t500)
    var t501_lhs uint16 = 513
    var t501_rhs uint16 = 256
    var t501 uint16 = t501_lhs % t501_rhs
    println__T_uint16(t501)
    var t502_lhs uint16 = 3855
    var t502_rhs uint16 = 255
    var t502 uint16 = t502_lhs & t502_rhs
    println__T_uint16(t502)
    var t503_lhs uint16 = 3840
    var t503_rhs uint16 = 15
    var t503 uint16 = t503_lhs | t503_rhs
    println__T_uint16(t503)
    var t504_lhs uint16 = 43690
    var t504_rhs uint16 = 3855
    var t504 uint16 = t504_lhs ^ t504_rhs
    println__T_uint16(t504)
    var t505_lhs uint16 = 1
    var t505_rhs int = 15
    var t505 uint16 = t505_lhs << t505_rhs
    println__T_uint16(t505)
    var t506_lhs uint16 = 32768
    var t506_rhs int = 15
    var t506 uint16 = t506_lhs >> t506_rhs
    println__T_uint16(t506)
    var t507_operand uint16 = 0
    var t507 uint16 = ^t507_operand
    println__T_uint16(t507)
    var t508_lhs uint32 = 1000000001
    var t508_rhs uint32 = 1000
    var t508 uint32 = t508_lhs % t508_rhs
    println__T_uint32(t508)
    var t509_lhs uint32 = 4042322160
    var t509_rhs uint32 = 252645135
    var t509 uint32 = t509_lhs & t509_rhs
    println__T_uint32(t509)
    var t510_lhs uint32 = 4042322160
    var t510_rhs uint32 = 252645135
    var t510 uint32 = t510_lhs | t510_rhs
    println__T_uint32(t510)
    var t511_lhs uint32 = 4042322160
    var t511_rhs uint32 = 252645135
    var t511 uint32 = t511_lhs ^ t511_rhs
    println__T_uint32(t511)
    var t512_lhs uint32 = 1
    var t512_rhs int = 31
    var t512 uint32 = t512_lhs << t512_rhs
    println__T_uint32(t512)
    var t513_lhs uint32 = 2147483648
    var t513_rhs int = 31
    var t513 uint32 = t513_lhs >> t513_rhs
    println__T_uint32(t513)
    var t514_operand uint32 = 0
    var t514 uint32 = ^t514_operand
    println__T_uint32(t514)
    var t515_lhs uint64 = 1000000000001
    var t515_rhs uint64 = 1000
    var t515 uint64 = t515_lhs % t515_rhs
    println__T_uint64(t515)
    var t516_lhs uint64 = 17361641481138401520
    var t516_rhs uint64 = 1085102592571150095
    var t516 uint64 = t516_lhs & t516_rhs
    println__T_uint64(t516)
    var t517_lhs uint64 = 17361641481138401520
    var t517_rhs uint64 = 1085102592571150095
    var t517 uint64 = t517_lhs | t517_rhs
    println__T_uint64(t517)
    var t518_lhs uint64 = 17361641481138401520
    var t518_rhs uint64 = 1085102592571150095
    var t518 uint64 = t518_lhs ^ t518_rhs
    println__T_uint64(t518)
    var t519_lhs uint64 = 1
    var t519_rhs int = 63
    var t519 uint64 = t519_lhs << t519_rhs
    println__T_uint64(t519)
    var t520_lhs uint64 = 9223372036854775808
    var t520_rhs int = 63
    var t520 uint64 = t520_lhs >> t520_rhs
    println__T_uint64(t520)
    var t521_operand uint64 = 0
    var t521 uint64 = ^t521_operand
    println__T_uint64(t521)
    return struct{}{}
}

func signed_ops() struct{} {
    var t524_lhs int8 = -13
    var t524_rhs int8 = 5
    var t524 int8 = t524_lhs % t524_rhs
    println__T_int8(t524)
    var t525_lhs int8 = -8
    var t525_rhs int = 2
    var t525 int8 = t525_lhs >> t525_rhs
    println__T_int8(t525)
    var t526_lhs int8 = 1
    var t526_rhs int = 6
    var t526 int8 = t526_lhs << t526_rhs
    println__T_int8(t526)
    var t527_operand int8 = 0
    var t527 int8 = ^t527_operand
    println__T_int8(t527)
    var t528_lhs int8 = -1
    var t528_rhs int = 7
    var t528 int8 = t528_lhs >> t528_rhs
    println__T_int8(t528)
    var t529_lhs int16 = -513
    var t529_rhs int16 = 256
    var t529 int16 = t529_lhs % t529_rhs
    println__T_int16(t529)
    var t530 int16 = -32767 - 1
    var t531_rhs int = 15
    var t531 int16 = t530 >> t531_rhs
    println__T_int16(t531)
    var t532_lhs int16 = 1
    var t532_rhs int = 14
    var t532 int16 = t532_lhs << t532_rhs
    println__T_int16(t532)
    var t533_operand int16 = 255
    var t533 int16 = ^t533_operand
    println__T_int16(t533)
    var t534_lhs int32 = -1000000001
    var t534_rhs int32 = 1000
    var t534 int32 = t534_lhs % t534_rhs
    println__T_int32(t534)
    var t535 int32 = -2147483647 - 1
    var t536_rhs int = 31
    var t536 int32 = t535 >> t536_rhs
    println__T_int32(t536)
    var t537_lhs int32 = 1
    var t537_rhs int = 30
    var t537 int32 = t537_lhs << t537_rhs
    println__T_int32(t537)
    var t538_operand int32 = 65535
    var t538 int32 = ^t538_operand
    println__T_int32(t538)
    var t539_lhs int64 = -1000000000001
    var t539_rhs int64 = 1000
    var t539 int64 = t539_lhs % t539_rhs
    println__T_int64(t539)
    var t540_lhs int64 = -9223372036854775807
    var t540_rhs int = 62
    var t540 int64 = t540_lhs >> t540_rhs
    println__T_int64(t540)
    var t541_lhs int64 = 1
    var t541_rhs int = 62
    var t541 int64 = t541_lhs << t541_rhs
    println__T_int64(t541)
    var t542_operand int64 = 4294967295
    var t542 int64 = ^t542_operand
    println__T_int64(t542)
    return struct{}{}
}

func precedence() struct{} {
    var t545_lhs uint8 = 3
    var t545_rhs uint8 = 1
    var t545 uint8 = t545_lhs & t545_rhs
    var t546_lhs uint8 = 2
    var t546 uint8 = t546_lhs ^ t545
    var t547_lhs uint8 = 1
    var t547 uint8 = t547_lhs | t546
    println__T_uint8(t547)
    var t548 int = 2 + 1
    var t549_lhs uint8 = 1
    var t549 uint8 = t549_lhs << t548
    println__T_uint8(t549)
    var t550_lhs int = 1
    var t550_rhs int = 2
    var t550 int = t550_lhs | t550_rhs
    var t551 bool = t550 == 3
    var t552 string
    var inline792 string = _goml_runtime_core_bool_to_string(t551)
    t552 = inline792
    var inline789 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t552)
    _goml_runtime_core_string_println(inline789)
    var t553_lhs int = 8
    var t553_rhs int = 1
    var t553 int = t553_lhs >> t553_rhs
    var t554 bool = t553 < 5
    var t555 string
    var inline787 string = _goml_runtime_core_bool_to_string(t554)
    t555 = inline787
    var inline784 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t555)
    _goml_runtime_core_string_println(inline784)
    var t556_operand uint8 = 1
    var t556 uint8 = ^t556_operand
    var t557_rhs uint8 = 15
    var t557 uint8 = t556 & t557_rhs
    println__T_uint8(t557)
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
    var t560 uint8 = uint8(uint16(five_eleven__8))
    println__T_uint8(t560)
    var t561 uint8 = uint8(uint16(two_fifty_six__9))
    println__T_uint8(t561)
    var t562 uint8 = uint8(int16(negative_one_i16__10))
    println__T_uint8(t562)
    var t563 int8 = int8(uint8(two_fifty_five__11))
    println__T_int8(t563)
    var t564 int8 = int8(uint8(one_twenty_eight__12))
    println__T_int8(t564)
    var t565 int8 = int8(int16(negative_one_twenty_nine__13))
    println__T_int8(t565)
    var t566 int16 = int16(uint16(max_u16__14))
    println__T_int16(t566)
    var t567 uint16 = uint16(int32(negative_one_i32__15))
    println__T_uint16(t567)
    var t568 uint64 = uint64(int8(negative_one_i8__16))
    println__T_uint64(t568)
    var t569 int32 = int32(uint64(max_u64__17))
    println__T_int32(t569)
    var t570 uint32 = uint32(uint8(sixty_five__18))
    println__T_uint32(t570)
    var t571 int64 = int64(uint32(max_u32__19))
    println__T_int64(t571)
    var t572_source rune = 65
    var t572 uint32 = uint32(rune(t572_source))
    println__T_uint32(t572)
    var mtmp469 Option__char
    var inline809 uint32 = 128512
    var inline810 Option__char = __goml_builtin_char_from_uint32(inline809)
    mtmp469 = inline810
    switch mtmp469.(type) {
    case None:
        var inline798 string = "invalid"
        var inline799 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline798)
        _goml_runtime_core_string_println(inline799)
    case Some:
        var x470 rune = mtmp469.(Some)._0
        var t578 string
        var inline805 string = char_to_string(x470)
        t578 = inline805
        var inline802 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t578)
        _goml_runtime_core_string_println(inline802)
    default:
        panic("non-exhaustive match")
    }
    var t574 uint8 = uint8(uint16(three_hundred__20))
    var t575 uint32 = uint32(uint8(t574))
    println__T_uint32(t575)
    return struct{}{}
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t588 uint8
    var inline840 uint8 = 10
    var inline841 uint8 = ^inline840
    var inline842_rhs uint8 = 15
    var inline842 uint8 = inline841 & inline842_rhs
    var inline843_lhs uint8 = 1
    var inline843_rhs int = 4
    var inline843 uint8 = inline843_lhs << inline843_rhs
    var inline844_rhs uint8 = 31
    var inline844 uint8 = inline843 % inline844_rhs
    var inline845 uint8 = inline842 | inline844
    t588 = inline845
    println__T_uint8(t588)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t591 string
    var inline847 string = _goml_runtime_core_uint8_to_string(value__1)
    t591 = inline847
    _goml_runtime_core_string_println(t591)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t594 string
    var inline849 string = _goml_runtime_core_uint16_to_string(value__1)
    t594 = inline849
    _goml_runtime_core_string_println(t594)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t597 string
    var inline851 string = _goml_runtime_core_uint32_to_string(value__1)
    t597 = inline851
    _goml_runtime_core_string_println(t597)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t600 string
    var inline853 string = _goml_runtime_core_uint64_to_string(value__1)
    t600 = inline853
    _goml_runtime_core_string_println(t600)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t603 string
    var inline855 string = _goml_runtime_core_int8_to_string(value__1)
    t603 = inline855
    _goml_runtime_core_string_println(t603)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t606 string
    var inline857 string = _goml_runtime_core_int16_to_string(value__1)
    t606 = inline857
    _goml_runtime_core_string_println(t606)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t609 string
    var inline859 string = _goml_runtime_core_int32_to_string(value__1)
    t609 = inline859
    _goml_runtime_core_string_println(t609)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t612 string
    var inline861 string = _goml_runtime_core_int64_to_string(value__1)
    t612 = inline861
    _goml_runtime_core_string_println(t612)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__156 uint8) string {
    var t628 string = _goml_runtime_core_uint8_to_string(self__156)
    return t628
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t656 bool
    var inline875 bool = value__30 <= 1114111
    if inline875 {
        var inline876 bool = value__30 >= 55296
        var inline878 bool
        if inline876 {
            var inline880 bool = value__30 <= 57343
            inline878 = inline880
        } else {
            inline878 = false
        }
        var inline879 bool = !inline878
        t656 = inline879
    } else {
        t656 = false
    }
    if t656 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t657 Option__char = Some{
            _0: x24,
        }
        return t657
    } else {
        return None{}
    }
}

func char_to_string(value__29 rune) string {
    var t662 uint32 = uint32(rune(value__29))
    var t663 bool
    var inline882 bool = t662 <= 1114111
    if inline882 {
        var inline883 bool = t662 >= 55296
        var inline885 bool
        if inline883 {
            var inline887 bool = t662 <= 57343
            inline885 = inline887
        } else {
            inline885 = false
        }
        var inline886 bool = !inline885
        t663 = inline886
    } else {
        t663 = false
    }
    if t663 {
        var t664 string = _goml_runtime_core_char_to_string(value__29)
        return t664
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
