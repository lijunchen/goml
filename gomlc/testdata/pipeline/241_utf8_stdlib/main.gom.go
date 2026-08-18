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

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_4char_3int struct {
    _0 rune
    _1 int
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Ordering int32

type _goml_m_Option_____o_char_c_int_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func check_utf8(bytes__0 *_goml_vec_uint8, expected__1 bool) struct{} {
    var expected_length__2 int
    var inline712 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline712
    var mtmp408 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x409 bool = mtmp408._0
    var x410 string = mtmp408._1
    var t449 bool = x409 == expected__1
    var inline709 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t449)
    _goml_runtime_core_string_println(inline709)
    if x409 {
        var t451 int
        var inline704 int = _goml_runtime_core_string_len(x410)
        t451 = inline704
        var t452 bool = t451 == expected_length__2
        var inline701 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t452)
        _goml_runtime_core_string_println(inline701)
        return struct{}{}
    } else {
        var t454 bool = x410 == ""
        var inline706 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t454)
        _goml_runtime_core_string_println(inline706)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp412 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x413 bool = mtmp412._0
    var x414 string = mtmp412._1
    var inline739 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x413)
    _goml_runtime_core_string_println(inline739)
    var commute_field847 Tuple2_4char_3int
    var inline728 int = 0
    var inline729 Tuple3_4bool_4char_3int = string_decode_utf8_at(x414, inline728)
    var inline730 bool = inline729._0
    var inline731 rune = inline729._1
    var inline732 int = inline729._2
    if inline730 {
        var inline736 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline731,
            _1: inline732,
        }
        commute_field847 = inline736
        var x420 rune = commute_field847._0
        var x421 int = commute_field847._1
        var t459 uint32 = uint32(rune(x420))
        var t460 bool = t459 == expected__6
        var inline725 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t460)
        _goml_runtime_core_string_println(inline725)
        var t461 bool = x421 == expected_width__7
        var inline722 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t461)
        _goml_runtime_core_string_println(inline722)
        return struct{}{}
    } else {
        var inline718 bool = false
        var inline719 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline718)
        _goml_runtime_core_string_println(inline719)
        var inline714 bool = false
        var inline715 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline714)
        _goml_runtime_core_string_println(inline715)
        return struct{}{}
    }
}

func main0() struct{} {
    var t464 [0]uint8 = [0]uint8{}
    var t465 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(t464)
    check_utf8(t465, true)
    var t466 [1]uint8 = [1]uint8{0}
    var t467 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t466)
    check_utf8(t467, true)
    var t468 [1]uint8 = [1]uint8{127}
    var t469 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t468)
    check_utf8(t469, true)
    var t470 [2]uint8 = [2]uint8{194, 128}
    var t471 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t470)
    check_scalar(t471, 128, 2)
    var t472 [2]uint8 = [2]uint8{223, 191}
    var t473 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t472)
    check_scalar(t473, 2047, 2)
    var t474 [3]uint8 = [3]uint8{224, 160, 128}
    var t475 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t474)
    check_scalar(t475, 2048, 3)
    var t476 [3]uint8 = [3]uint8{237, 159, 191}
    var t477 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t476)
    check_scalar(t477, 55295, 3)
    var t478 [3]uint8 = [3]uint8{238, 128, 128}
    var t479 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t478)
    check_scalar(t479, 57344, 3)
    var t480 [3]uint8 = [3]uint8{239, 191, 189}
    var t481 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t480)
    check_scalar(t481, 65533, 3)
    var t482 [3]uint8 = [3]uint8{239, 191, 191}
    var t483 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t482)
    check_scalar(t483, 65535, 3)
    var t484 [4]uint8 = [4]uint8{240, 144, 128, 128}
    var t485 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t484)
    check_scalar(t485, 65536, 4)
    var t486 [4]uint8 = [4]uint8{244, 143, 191, 191}
    var t487 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t486)
    check_scalar(t487, 1114111, 4)
    var t488 [1]uint8 = [1]uint8{128}
    var t489 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t488)
    check_utf8(t489, false)
    var t490 [1]uint8 = [1]uint8{191}
    var t491 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t490)
    check_utf8(t491, false)
    var t492 [2]uint8 = [2]uint8{192, 128}
    var t493 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t492)
    check_utf8(t493, false)
    var t494 [2]uint8 = [2]uint8{193, 191}
    var t495 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t494)
    check_utf8(t495, false)
    var t496 [1]uint8 = [1]uint8{194}
    var t497 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t496)
    check_utf8(t497, false)
    var t498 [2]uint8 = [2]uint8{194, 127}
    var t499 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t498)
    check_utf8(t499, false)
    var t500 [3]uint8 = [3]uint8{224, 159, 191}
    var t501 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t500)
    check_utf8(t501, false)
    var t502 [2]uint8 = [2]uint8{225, 128}
    var t503 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t502)
    check_utf8(t503, false)
    var t504 [3]uint8 = [3]uint8{225, 128, 127}
    var t505 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t504)
    check_utf8(t505, false)
    var t506 [3]uint8 = [3]uint8{237, 160, 128}
    var t507 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t506)
    check_utf8(t507, false)
    var t508 [4]uint8 = [4]uint8{240, 143, 191, 191}
    var t509 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t508)
    check_utf8(t509, false)
    var t510 [3]uint8 = [3]uint8{240, 144, 128}
    var t511 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t510)
    var inline772 bool = false
    var inline773 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(t511)
    var inline774 Tuple2_4bool_6string = string_from_utf8(t511)
    var inline775 bool = inline774._0
    var inline776 string = inline774._1
    var inline779 bool = inline775 == inline772
    println__T_bool(inline779)
    if inline775 {
        var inline781 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline776)
        var inline782 bool = inline781 == inline773
        println__T_bool(inline782)
    } else {
        var inline784 bool = inline776 == ""
        println__T_bool(inline784)
    }
    var t512 [4]uint8 = [4]uint8{244, 144, 128, 128}
    var t513 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t512)
    var inline757 bool = false
    var inline758 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(t513)
    var inline759 Tuple2_4bool_6string = string_from_utf8(t513)
    var inline760 bool = inline759._0
    var inline761 string = inline759._1
    var inline764 bool = inline760 == inline757
    println__T_bool(inline764)
    if inline760 {
        var inline766 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline761)
        var inline767 bool = inline766 == inline758
        println__T_bool(inline767)
    } else {
        var inline769 bool = inline761 == ""
        println__T_bool(inline769)
    }
    var t514 [4]uint8 = [4]uint8{245, 128, 128, 128}
    var t515 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t514)
    var inline742 bool = false
    var inline743 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(t515)
    var inline744 Tuple2_4bool_6string = string_from_utf8(t515)
    var inline745 bool = inline744._0
    var inline746 string = inline744._1
    var inline749 bool = inline745 == inline742
    println__T_bool(inline749)
    if inline745 {
        var inline751 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline746)
        var inline752 bool = inline751 == inline743
        println__T_bool(inline752)
        return struct{}{}
    } else {
        var inline754 bool = inline746 == ""
        println__T_bool(inline754)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__273 *_goml_vec_uint8) int {
    var t519 int = vec_len__Vec_5uint8(self__273)
    return t519
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop524:
    for {
        var t525 int
        var inline787 int = _goml_runtime_core_string_len(x12)
        t525 = inline787
        var t526 bool = index__26 < t525
        if t526 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t528 int = compound_old17 + x16
                index__26 = t528
                continue
            } else {
                var t530 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t530
            }
        } else {
            break Loop_loop524
        }
    }
    var t523 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t523
}

func println__T_bool(value__1 bool) struct{} {
    var t532 string
    var inline789 string = _goml_runtime_core_bool_to_string(value__1)
    t532 = inline789
    _goml_runtime_core_string_println(t532)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t536 int = _goml_runtime_core_string_len(self__36)
    return t536
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t661 bool = index__6 < 0
    var jp659 bool
    if t661 {
        jp659 = true
    } else {
        var t662 bool = index__6 >= length__7
        jp659 = t662
    }
    if jp659 {
        var inline791 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline791
    } else {
        var t546 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t546))
        var t549 bool = first__8 < 128
        if t549 {
            var inline793 int = 1
            var inline794 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline794._tag {
            case 0:
                var inline795 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline795
            case 1:
                var inline796 rune = inline794._v1_0
                var inline798 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline796,
                    _2: inline793,
                }
                return inline798
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t553 bool = first__8 < 194
            if t553 {
                var inline800 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline800
            } else {
                var t557 bool = first__8 < 224
                if t557 {
                    var t570 int = length__7 - index__6
                    var t571 bool = t570 < 2
                    if t571 {
                        var inline802 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline802
                    } else {
                        var t559 int = index__6 + 1
                        var t560 uint8
                        var inline816 uint8 = _goml_runtime_core_string_byte_get(value__5, t559)
                        t560 = inline816
                        var second__9 uint32 = uint32(uint8(t560))
                        var t563 bool
                        var inline813 bool = second__9 < 128
                        if inline813 {
                            t563 = true
                        } else {
                            var inline814 bool = second__9 > 191
                            t563 = inline814
                        }
                        if t563 {
                            var inline804 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline804
                        } else {
                            var t565_rhs uint32 = 31
                            var t565 uint32 = first__8 & t565_rhs
                            var t566_rhs int = 6
                            var t566 uint32 = t565 << t566_rhs
                            var t567_rhs uint32 = 63
                            var t567 uint32 = second__9 & t567_rhs
                            var t568 uint32 = t566 | t567
                            var inline806 int = 2
                            var inline807 Option__char = __goml_builtin_char_from_uint32(t568)
                            switch inline807._tag {
                            case 0:
                                var inline808 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline808
                            case 1:
                                var inline809 rune = inline807._v1_0
                                var inline811 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline809,
                                    _2: inline806,
                                }
                                return inline811
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t575 bool = first__8 < 240
                    if t575 {
                        var t608 int = length__7 - index__6
                        var t609 bool = t608 < 3
                        if t609 {
                            var inline818 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline818
                        } else {
                            var t577 int = index__6 + 1
                            var t578 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t577)
                            var second__10 uint32 = uint32(uint8(t578))
                            var t579 int = index__6 + 2
                            var t580 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t579)
                            var third__11 uint32 = uint32(uint8(t580))
                            var t606 bool = utf8_invalid_continuation(second__10)
                            var jp601 bool
                            if t606 {
                                jp601 = true
                            } else {
                                var inline820 bool = third__11 < 128
                                if inline820 {
                                    jp601 = true
                                } else {
                                    var inline821 bool = third__11 > 191
                                    jp601 = inline821
                                }
                            }
                            var jp595 bool
                            if jp601 {
                                jp595 = true
                            } else {
                                var t604 bool = first__8 == 224
                                if t604 {
                                    var t605 bool = second__10 < 160
                                    jp595 = t605
                                } else {
                                    jp595 = false
                                }
                            }
                            var jp584 bool
                            if jp595 {
                                jp584 = true
                            } else {
                                var t598 bool = first__8 == 237
                                if t598 {
                                    var t599 bool = second__10 >= 160
                                    jp584 = t599
                                } else {
                                    jp584 = false
                                }
                            }
                            if jp584 {
                                var inline823 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline823
                            } else {
                                var t586_rhs uint32 = 15
                                var t586 uint32 = first__8 & t586_rhs
                                var t587_rhs int = 12
                                var t587 uint32 = t586 << t587_rhs
                                var t588_rhs uint32 = 63
                                var t588 uint32 = second__10 & t588_rhs
                                var t589_rhs int = 6
                                var t589 uint32 = t588 << t589_rhs
                                var t590 uint32 = t587 | t589
                                var t591_rhs uint32 = 63
                                var t591 uint32 = third__11 & t591_rhs
                                var t592 uint32 = t590 | t591
                                var inline825 int = 3
                                var inline826 Option__char = __goml_builtin_char_from_uint32(t592)
                                switch inline826._tag {
                                case 0:
                                    var inline827 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline827
                                case 1:
                                    var inline828 rune = inline826._v1_0
                                    var inline830 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline828,
                                        _2: inline825,
                                    }
                                    return inline830
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t613 bool = first__8 < 245
                        if t613 {
                            var t654 int = length__7 - index__6
                            var t655 bool = t654 < 4
                            if t655 {
                                var t656 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t656
                            } else {
                                var t615 int = index__6 + 1
                                var t616 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t615)
                                var second__12 uint32 = uint32(uint8(t616))
                                var t617 int = index__6 + 2
                                var t618 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t617)
                                var third__13 uint32 = uint32(uint8(t618))
                                var t619 int = index__6 + 3
                                var t620 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t619)
                                var fourth__14 uint32 = uint32(uint8(t620))
                                var t652 bool = utf8_invalid_continuation(second__12)
                                var jp650 bool
                                if t652 {
                                    jp650 = true
                                } else {
                                    var t653 bool = utf8_invalid_continuation(third__13)
                                    jp650 = t653
                                }
                                var jp644 bool
                                if jp650 {
                                    jp644 = true
                                } else {
                                    var t651 bool = utf8_invalid_continuation(fourth__14)
                                    jp644 = t651
                                }
                                var jp638 bool
                                if jp644 {
                                    jp638 = true
                                } else {
                                    var t647 bool = first__8 == 240
                                    if t647 {
                                        var t648 bool = second__12 < 144
                                        jp638 = t648
                                    } else {
                                        jp638 = false
                                    }
                                }
                                var jp624 bool
                                if jp638 {
                                    jp624 = true
                                } else {
                                    var t641 bool = first__8 == 244
                                    if t641 {
                                        var t642 bool = second__12 > 143
                                        jp624 = t642
                                    } else {
                                        jp624 = false
                                    }
                                }
                                if jp624 {
                                    var t625 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t625
                                } else {
                                    var t626_rhs uint32 = 7
                                    var t626 uint32 = first__8 & t626_rhs
                                    var t627_rhs int = 18
                                    var t627 uint32 = t626 << t627_rhs
                                    var t628_rhs uint32 = 63
                                    var t628 uint32 = second__12 & t628_rhs
                                    var t629_rhs int = 12
                                    var t629 uint32 = t628 << t629_rhs
                                    var t630 uint32 = t627 | t629
                                    var t631_rhs uint32 = 63
                                    var t631 uint32 = third__13 & t631_rhs
                                    var t632_rhs int = 6
                                    var t632 uint32 = t631 << t632_rhs
                                    var t633 uint32 = t630 | t632
                                    var t634_rhs uint32 = 63
                                    var t634 uint32 = fourth__14 & t634_rhs
                                    var t635 uint32 = t633 | t634
                                    var t636 Tuple3_4bool_4char_3int = utf8_valid_decode(t635, 4)
                                    return t636
                                }
                            }
                        } else {
                            var t657 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t657
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t665 string = _goml_runtime_core_bool_to_string(self__148)
    return t665
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t668 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t668
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t671 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t671
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field850 rune
    var inline834 bool = utf8_valid_scalar(value__0)
    if inline834 {
        var inline835 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline836 rune = inline835._1
        commute_field850 = inline836
        var t677 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field850,
            _2: width__1,
        }
        return t677
    } else {
        var inline832 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline832
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t682 bool = value__3 < 128
    if t682 {
        return true
    } else {
        var t683 bool = value__3 > 191
        return t683
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t688 bool
    var inline840 bool = value__30 <= 1114111
    if inline840 {
        var inline841 bool = value__30 >= 55296
        var inline843 bool
        if inline841 {
            var inline845 bool = value__30 <= 57343
            inline843 = inline845
        } else {
            inline843 = false
        }
        var inline844 bool = !inline843
        t688 = inline844
    } else {
        t688 = false
    }
    if t688 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t689 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t689
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t694 bool = value__4 <= 1114111
    if t694 {
        var t698 bool = value__4 >= 55296
        var jp696 bool
        if t698 {
            var t699 bool = value__4 <= 57343
            jp696 = t699
        } else {
            jp696 = false
        }
        var t697 bool = !jp696
        return t697
    } else {
        return false
    }
}

func main() {
    main0()
}
