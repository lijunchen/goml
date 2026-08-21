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
    var inline715 int = vec_len__Vec_5uint8(bytes__0)
    expected_length__2 = inline715
    var mtmp411 Tuple2_4bool_6string = string_from_utf8(bytes__0)
    var x412 bool = mtmp411._0
    var x413 string = mtmp411._1
    var t452 bool = x412 == expected__1
    var inline712 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t452)
    _goml_runtime_core_string_println(inline712)
    if x412 {
        var t454 int
        var inline707 int = _goml_runtime_core_string_len(x413)
        t454 = inline707
        var t455 bool = t454 == expected_length__2
        var inline704 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t455)
        _goml_runtime_core_string_println(inline704)
        return struct{}{}
    } else {
        var t457 bool = x413 == ""
        var inline709 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t457)
        _goml_runtime_core_string_println(inline709)
        return struct{}{}
    }
}

func check_scalar(bytes__5 *_goml_vec_uint8, expected__6 uint32, expected_width__7 int) struct{} {
    var mtmp415 Tuple2_4bool_6string = string_from_utf8(bytes__5)
    var x416 bool = mtmp415._0
    var x417 string = mtmp415._1
    var inline742 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x416)
    _goml_runtime_core_string_println(inline742)
    var commute_field850 Tuple2_4char_3int
    var inline731 int = 0
    var inline732 Tuple3_4bool_4char_3int = string_decode_utf8_at(x417, inline731)
    var inline733 bool = inline732._0
    var inline734 rune = inline732._1
    var inline735 int = inline732._2
    if inline733 {
        var inline739 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline734,
            _1: inline735,
        }
        commute_field850 = inline739
        var x423 rune = commute_field850._0
        var x424 int = commute_field850._1
        var t462 uint32 = uint32(rune(x423))
        var t463 bool = t462 == expected__6
        var inline728 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t463)
        _goml_runtime_core_string_println(inline728)
        var t464 bool = x424 == expected_width__7
        var inline725 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t464)
        _goml_runtime_core_string_println(inline725)
        return struct{}{}
    } else {
        var inline721 bool = false
        var inline722 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline721)
        _goml_runtime_core_string_println(inline722)
        var inline717 bool = false
        var inline718 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline717)
        _goml_runtime_core_string_println(inline718)
        return struct{}{}
    }
}

func main0() struct{} {
    var t467 [0]uint8 = [0]uint8{}
    var t468 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(t467)
    check_utf8(t468, true)
    var t469 [1]uint8 = [1]uint8{0}
    var t470 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t469)
    check_utf8(t470, true)
    var t471 [1]uint8 = [1]uint8{127}
    var t472 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t471)
    check_utf8(t472, true)
    var t473 [2]uint8 = [2]uint8{194, 128}
    var t474 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t473)
    check_scalar(t474, 128, 2)
    var t475 [2]uint8 = [2]uint8{223, 191}
    var t476 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t475)
    check_scalar(t476, 2047, 2)
    var t477 [3]uint8 = [3]uint8{224, 160, 128}
    var t478 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t477)
    check_scalar(t478, 2048, 3)
    var t479 [3]uint8 = [3]uint8{237, 159, 191}
    var t480 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t479)
    check_scalar(t480, 55295, 3)
    var t481 [3]uint8 = [3]uint8{238, 128, 128}
    var t482 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t481)
    check_scalar(t482, 57344, 3)
    var t483 [3]uint8 = [3]uint8{239, 191, 189}
    var t484 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t483)
    check_scalar(t484, 65533, 3)
    var t485 [3]uint8 = [3]uint8{239, 191, 191}
    var t486 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t485)
    check_scalar(t486, 65535, 3)
    var t487 [4]uint8 = [4]uint8{240, 144, 128, 128}
    var t488 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t487)
    check_scalar(t488, 65536, 4)
    var t489 [4]uint8 = [4]uint8{244, 143, 191, 191}
    var t490 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t489)
    check_scalar(t490, 1114111, 4)
    var t491 [1]uint8 = [1]uint8{128}
    var t492 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t491)
    check_utf8(t492, false)
    var t493 [1]uint8 = [1]uint8{191}
    var t494 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t493)
    check_utf8(t494, false)
    var t495 [2]uint8 = [2]uint8{192, 128}
    var t496 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t495)
    check_utf8(t496, false)
    var t497 [2]uint8 = [2]uint8{193, 191}
    var t498 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t497)
    check_utf8(t498, false)
    var t499 [1]uint8 = [1]uint8{194}
    var t500 *_goml_vec_uint8 = func(values [1]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [1]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t499)
    check_utf8(t500, false)
    var t501 [2]uint8 = [2]uint8{194, 127}
    var t502 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t501)
    check_utf8(t502, false)
    var t503 [3]uint8 = [3]uint8{224, 159, 191}
    var t504 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t503)
    check_utf8(t504, false)
    var t505 [2]uint8 = [2]uint8{225, 128}
    var t506 *_goml_vec_uint8 = func(values [2]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [2]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t505)
    check_utf8(t506, false)
    var t507 [3]uint8 = [3]uint8{225, 128, 127}
    var t508 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t507)
    check_utf8(t508, false)
    var t509 [3]uint8 = [3]uint8{237, 160, 128}
    var t510 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t509)
    check_utf8(t510, false)
    var t511 [4]uint8 = [4]uint8{240, 143, 191, 191}
    var t512 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t511)
    check_utf8(t512, false)
    var t513 [3]uint8 = [3]uint8{240, 144, 128}
    var t514 *_goml_vec_uint8 = func(values [3]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [3]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t513)
    var inline775 bool = false
    var inline776 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(t514)
    var inline777 Tuple2_4bool_6string = string_from_utf8(t514)
    var inline778 bool = inline777._0
    var inline779 string = inline777._1
    var inline782 bool = inline778 == inline775
    println__T_bool(inline782)
    if inline778 {
        var inline784 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline779)
        var inline785 bool = inline784 == inline776
        println__T_bool(inline785)
    } else {
        var inline787 bool = inline779 == ""
        println__T_bool(inline787)
    }
    var t515 [4]uint8 = [4]uint8{244, 144, 128, 128}
    var t516 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t515)
    var inline760 bool = false
    var inline761 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(t516)
    var inline762 Tuple2_4bool_6string = string_from_utf8(t516)
    var inline763 bool = inline762._0
    var inline764 string = inline762._1
    var inline767 bool = inline763 == inline760
    println__T_bool(inline767)
    if inline763 {
        var inline769 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline764)
        var inline770 bool = inline769 == inline761
        println__T_bool(inline770)
    } else {
        var inline772 bool = inline764 == ""
        println__T_bool(inline772)
    }
    var t517 [4]uint8 = [4]uint8{245, 128, 128, 128}
    var t518 *_goml_vec_uint8 = func(values [4]uint8) *_goml_vec_uint8 {
        var storage struct {
            vector _goml_vec_uint8
            values [4]uint8
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t517)
    var inline745 bool = false
    var inline746 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(t518)
    var inline747 Tuple2_4bool_6string = string_from_utf8(t518)
    var inline748 bool = inline747._0
    var inline749 string = inline747._1
    var inline752 bool = inline748 == inline745
    println__T_bool(inline752)
    if inline748 {
        var inline754 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline749)
        var inline755 bool = inline754 == inline746
        println__T_bool(inline755)
        return struct{}{}
    } else {
        var inline757 bool = inline749 == ""
        println__T_bool(inline757)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__273 *_goml_vec_uint8) int {
    var t522 int = vec_len__Vec_5uint8(self__273)
    return t522
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop527:
    for {
        var t528 int
        var inline790 int = _goml_runtime_core_string_len(x12)
        t528 = inline790
        var t529 bool = index__26 < t528
        if t529 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t531 int = compound_old17 + x16
                index__26 = t531
                continue
            } else {
                var t533 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t533
            }
        } else {
            break Loop_loop527
        }
    }
    var t526 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t526
}

func println__T_bool(value__1 bool) struct{} {
    var t535 string
    var inline792 string = _goml_runtime_core_bool_to_string(value__1)
    t535 = inline792
    _goml_runtime_core_string_println(t535)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t539 int = _goml_runtime_core_string_len(self__36)
    return t539
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t664 bool = index__6 < 0
    var jp662 bool
    if t664 {
        jp662 = true
    } else {
        var t665 bool = index__6 >= length__7
        jp662 = t665
    }
    if jp662 {
        var inline794 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline794
    } else {
        var t549 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t549))
        var t552 bool = first__8 < 128
        if t552 {
            var inline796 int = 1
            var inline797 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline797._tag {
            case 0:
                var inline798 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline798
            case 1:
                var inline799 rune = inline797._v1_0
                var inline801 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline799,
                    _2: inline796,
                }
                return inline801
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t556 bool = first__8 < 194
            if t556 {
                var inline803 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline803
            } else {
                var t560 bool = first__8 < 224
                if t560 {
                    var t573 int = length__7 - index__6
                    var t574 bool = t573 < 2
                    if t574 {
                        var inline805 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline805
                    } else {
                        var t562 int = index__6 + 1
                        var t563 uint8
                        var inline819 uint8 = _goml_runtime_core_string_byte_get(value__5, t562)
                        t563 = inline819
                        var second__9 uint32 = uint32(uint8(t563))
                        var t566 bool
                        var inline816 bool = second__9 < 128
                        if inline816 {
                            t566 = true
                        } else {
                            var inline817 bool = second__9 > 191
                            t566 = inline817
                        }
                        if t566 {
                            var inline807 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline807
                        } else {
                            var t568_rhs uint32 = 31
                            var t568 uint32 = first__8 & t568_rhs
                            var t569_rhs int = 6
                            var t569 uint32 = t568 << t569_rhs
                            var t570_rhs uint32 = 63
                            var t570 uint32 = second__9 & t570_rhs
                            var t571 uint32 = t569 | t570
                            var inline809 int = 2
                            var inline810 Option__char = __goml_builtin_char_from_uint32(t571)
                            switch inline810._tag {
                            case 0:
                                var inline811 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline811
                            case 1:
                                var inline812 rune = inline810._v1_0
                                var inline814 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline812,
                                    _2: inline809,
                                }
                                return inline814
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t578 bool = first__8 < 240
                    if t578 {
                        var t611 int = length__7 - index__6
                        var t612 bool = t611 < 3
                        if t612 {
                            var inline821 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline821
                        } else {
                            var t580 int = index__6 + 1
                            var t581 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t580)
                            var second__10 uint32 = uint32(uint8(t581))
                            var t582 int = index__6 + 2
                            var t583 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t582)
                            var third__11 uint32 = uint32(uint8(t583))
                            var t609 bool = utf8_invalid_continuation(second__10)
                            var jp604 bool
                            if t609 {
                                jp604 = true
                            } else {
                                var inline823 bool = third__11 < 128
                                if inline823 {
                                    jp604 = true
                                } else {
                                    var inline824 bool = third__11 > 191
                                    jp604 = inline824
                                }
                            }
                            var jp598 bool
                            if jp604 {
                                jp598 = true
                            } else {
                                var t607 bool = first__8 == 224
                                if t607 {
                                    var t608 bool = second__10 < 160
                                    jp598 = t608
                                } else {
                                    jp598 = false
                                }
                            }
                            var jp587 bool
                            if jp598 {
                                jp587 = true
                            } else {
                                var t601 bool = first__8 == 237
                                if t601 {
                                    var t602 bool = second__10 >= 160
                                    jp587 = t602
                                } else {
                                    jp587 = false
                                }
                            }
                            if jp587 {
                                var inline826 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline826
                            } else {
                                var t589_rhs uint32 = 15
                                var t589 uint32 = first__8 & t589_rhs
                                var t590_rhs int = 12
                                var t590 uint32 = t589 << t590_rhs
                                var t591_rhs uint32 = 63
                                var t591 uint32 = second__10 & t591_rhs
                                var t592_rhs int = 6
                                var t592 uint32 = t591 << t592_rhs
                                var t593 uint32 = t590 | t592
                                var t594_rhs uint32 = 63
                                var t594 uint32 = third__11 & t594_rhs
                                var t595 uint32 = t593 | t594
                                var inline828 int = 3
                                var inline829 Option__char = __goml_builtin_char_from_uint32(t595)
                                switch inline829._tag {
                                case 0:
                                    var inline830 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline830
                                case 1:
                                    var inline831 rune = inline829._v1_0
                                    var inline833 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline831,
                                        _2: inline828,
                                    }
                                    return inline833
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t616 bool = first__8 < 245
                        if t616 {
                            var t657 int = length__7 - index__6
                            var t658 bool = t657 < 4
                            if t658 {
                                var t659 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t659
                            } else {
                                var t618 int = index__6 + 1
                                var t619 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t618)
                                var second__12 uint32 = uint32(uint8(t619))
                                var t620 int = index__6 + 2
                                var t621 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t620)
                                var third__13 uint32 = uint32(uint8(t621))
                                var t622 int = index__6 + 3
                                var t623 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t622)
                                var fourth__14 uint32 = uint32(uint8(t623))
                                var t655 bool = utf8_invalid_continuation(second__12)
                                var jp653 bool
                                if t655 {
                                    jp653 = true
                                } else {
                                    var t656 bool = utf8_invalid_continuation(third__13)
                                    jp653 = t656
                                }
                                var jp647 bool
                                if jp653 {
                                    jp647 = true
                                } else {
                                    var t654 bool = utf8_invalid_continuation(fourth__14)
                                    jp647 = t654
                                }
                                var jp641 bool
                                if jp647 {
                                    jp641 = true
                                } else {
                                    var t650 bool = first__8 == 240
                                    if t650 {
                                        var t651 bool = second__12 < 144
                                        jp641 = t651
                                    } else {
                                        jp641 = false
                                    }
                                }
                                var jp627 bool
                                if jp641 {
                                    jp627 = true
                                } else {
                                    var t644 bool = first__8 == 244
                                    if t644 {
                                        var t645 bool = second__12 > 143
                                        jp627 = t645
                                    } else {
                                        jp627 = false
                                    }
                                }
                                if jp627 {
                                    var t628 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t628
                                } else {
                                    var t629_rhs uint32 = 7
                                    var t629 uint32 = first__8 & t629_rhs
                                    var t630_rhs int = 18
                                    var t630 uint32 = t629 << t630_rhs
                                    var t631_rhs uint32 = 63
                                    var t631 uint32 = second__12 & t631_rhs
                                    var t632_rhs int = 12
                                    var t632 uint32 = t631 << t632_rhs
                                    var t633 uint32 = t630 | t632
                                    var t634_rhs uint32 = 63
                                    var t634 uint32 = third__13 & t634_rhs
                                    var t635_rhs int = 6
                                    var t635 uint32 = t634 << t635_rhs
                                    var t636 uint32 = t633 | t635
                                    var t637_rhs uint32 = 63
                                    var t637 uint32 = fourth__14 & t637_rhs
                                    var t638 uint32 = t636 | t637
                                    var t639 Tuple3_4bool_4char_3int = utf8_valid_decode(t638, 4)
                                    return t639
                                }
                            }
                        } else {
                            var t660 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t660
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t668 string = _goml_runtime_core_bool_to_string(self__148)
    return t668
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t671 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t671
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t674 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t674
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field853 rune
    var inline837 bool = utf8_valid_scalar(value__0)
    if inline837 {
        var inline838 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline839 rune = inline838._1
        commute_field853 = inline839
        var t680 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field853,
            _2: width__1,
        }
        return t680
    } else {
        var inline835 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline835
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t685 bool = value__3 < 128
    if t685 {
        return true
    } else {
        var t686 bool = value__3 > 191
        return t686
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t691 bool
    var inline843 bool = value__30 <= 1114111
    if inline843 {
        var inline844 bool = value__30 >= 55296
        var inline846 bool
        if inline844 {
            var inline848 bool = value__30 <= 57343
            inline846 = inline848
        } else {
            inline846 = false
        }
        var inline847 bool = !inline846
        t691 = inline847
    } else {
        t691 = false
    }
    if t691 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t692 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t692
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t697 bool = value__4 <= 1114111
    if t697 {
        var t701 bool = value__4 >= 55296
        var jp699 bool
        if t701 {
            var t702 bool = value__4 <= 57343
            jp699 = t702
        } else {
            jp699 = false
        }
        var t700 bool = !jp699
        return t700
    } else {
        return false
    }
}

func main() {
    main0()
}
