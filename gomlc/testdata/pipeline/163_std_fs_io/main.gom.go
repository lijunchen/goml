package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
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

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
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

func _goml_runtime_std_fs_read_bytes(path string) Tuple3_4bool_10Vec_5uint8_6string {
    var data []uint8
    var err error
    data, err = _goml_os.ReadFile(path)
    if err != nil {
        return Tuple3_4bool_10Vec_5uint8_6string{
            _0: false,
            _1: &_goml_vec_uint8{
                items: nil,
            },
            _2: err.Error(),
        }
    }
    return Tuple3_4bool_10Vec_5uint8_6string{
        _0: true,
        _1: &_goml_vec_uint8{
            items: data,
        },
        _2: "",
    }
}

func _goml_runtime_std_fs_write_bytes(path string, data *_goml_vec_uint8) Tuple2_4bool_6string {
    var err error = _goml_os.WriteFile(path, data.items, 0644)
    if err != nil {
        return Tuple2_4bool_6string{
            _0: false,
            _1: err.Error(),
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: "",
    }
}

func _goml_runtime_std_fs_file_exists(path string) bool {
    var err error
    _, err = _goml_os.Stat(path)
    return err == nil
}

func _goml_runtime_std_fs_read_dir(path string) Tuple3_4bool_11Vec_6string_6string {
    var entries []_goml_os.DirEntry
    var err error
    entries, err = _goml_os.ReadDir(path)
    if err != nil {
        return Tuple3_4bool_11Vec_6string_6string{
            _0: false,
            _1: &_goml_vec_string{
                items: nil,
            },
            _2: err.Error(),
        }
    }
    var names []string
    var i int = 0
    for {
        if i >= int(len(entries)) {
            break
        }
        var entry _goml_os.DirEntry = entries[i]
        names = append(names, entry.Name())
        i = i + 1
    }
    return Tuple3_4bool_11Vec_6string_6string{
        _0: true,
        _1: &_goml_vec_string{
            items: names,
        },
        _2: "",
    }
}

func _goml_runtime_std_io_println(value string) struct{} {
    _goml_fmt.Println(value)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_string struct {
    items []string
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_10Vec_5uint8_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 string
}

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
}

type Tuple3_4bool_11Vec_6string_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 string
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

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type Option__uint8 interface {
    isOption__uint8()
}

type Option__uint8_None struct {}

func (_ Option__uint8_None) isOption__uint8() {}

type Option__uint8_Some struct {
    _0 uint8
}

func (_ Option__uint8_Some) isOption__uint8() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

type _goml_m_Result____std_p_bytes_p_Bytes____string interface {
    is_goml_m_Result____std_p_bytes_p_Bytes____string()
}

type _goml_m_Result____std_p_bytes_p_Bytes____string_Ok struct {
    _0 _goml_m_std_p_bytes_p_Bytes
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____string_Ok) is_goml_m_Result____std_p_bytes_p_Bytes____string() {}

type _goml_m_Result____std_p_bytes_p_Bytes____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____string_Err) is_goml_m_Result____std_p_bytes_p_Bytes____string() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type _goml_m_Result____Vec_l_string_r_____string interface {
    is_goml_m_Result____Vec_l_string_r_____string()
}

type _goml_m_Result____Vec_l_string_r_____string_Ok struct {
    _0 *_goml_vec_string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Ok) is_goml_m_Result____Vec_l_string_r_____string() {}

type _goml_m_Result____Vec_l_string_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Err) is_goml_m_Result____Vec_l_string_r_____string() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(values__1 *_goml_vec_uint8) _goml_m_std_p_bytes_p_Bytes {
    var t194 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t194
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t197 *_goml_vec_uint8
    var inline596 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t197 = inline596
    var t198 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t197,
    }
    return t198
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var commute_field791 _goml_m_std_p_bytes_p_Bytes
    var commute_field793 string
    var inline632 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__0)
    var inline633 bool = inline632._0
    var inline634 *_goml_vec_uint8 = inline632._1
    var inline635 string = inline632._2
    if inline633 {
        var inline639 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline634)
        commute_field791 = inline639
        var inline623 *_goml_vec_uint8 = commute_field791.values
        var inline624 Tuple2_4bool_6string = string_from_utf8(inline623)
        var inline625 bool = inline624._0
        var inline626 string = inline624._1
        if inline625 {
            var inline629 Result__string__string = Result__string__string_Ok{
                _0: inline626,
            }
            return inline629
        } else {
            var inline630 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline630
        }
    } else {
        commute_field793 = inline635
        var t252 Result__string__string = Result__string__string_Err{
            _0: commute_field793,
        }
        return t252
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t266 *_goml_vec_uint8
    var inline657 *_goml_vec_uint8 = data__10.values
    t266 = inline657
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t266)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    if x8 {
        var t269 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t269
    } else {
        var t270 Result__unit__string = Result__unit__string_Err{
            _0: x9,
        }
        return t270
    }
}

func main0() struct{} {
    var inline711 string = "goml-std-test.txt"
    var inline712 string = "std-ok"
    var inline713 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline712)
    _goml_m_std_p_fs_p_write__bytes(inline711, inline713)
    var t362 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t363 string
    switch t362.(type) {
    case Result__string__string_Ok:
        var inline705 string = t362.(Result__string__string_Ok)._0
        t363 = inline705
    case Result__string__string_Err:
        var inline707 string = t362.(Result__string__string_Err)._0
        var inline709 string = "err " + inline707
        t363 = inline709
    default:
        panic("non-exhaustive match")
    }
    var inline702 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t363)
    _goml_runtime_std_io_println(inline702)
    var t364 bool
    var inline699 string = "goml-std-test.txt"
    var inline700 bool = _goml_runtime_std_fs_file_exists(inline699)
    t364 = inline700
    var t365 string
    var inline697 string = _goml_runtime_core_bool_to_string(t364)
    t365 = inline697
    var inline694 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t365)
    _goml_runtime_std_io_println(inline694)
    var t366 _goml_m_Result____Vec_l_string_r_____string
    var inline683 string = "."
    var inline684 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(inline683)
    var inline685 bool = inline684._0
    var inline686 *_goml_vec_string = inline684._1
    var inline687 string = inline684._2
    if inline685 {
        var inline691 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline686,
        }
        t366 = inline691
    } else {
        var inline692 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline687,
        }
        t366 = inline692
    }
    var t367 string
    switch t366.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline674 *_goml_vec_string = t366.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline676 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline674)
        var inline677 bool = inline676 > 0
        var inline678 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline677)
        t367 = inline678
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline679 string = t366.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline681 string = "err " + inline679
        t367 = inline681
    default:
        panic("non-exhaustive match")
    }
    var inline671 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t367)
    _goml_runtime_std_io_println(inline671)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop410:
    for {
        var t411 int
        var inline729 int = _goml_runtime_core_string_len(x12)
        t411 = inline729
        var t412 bool = index__26 < t411
        if t412 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t414 int = compound_old17 + x16
                index__26 = t414
                continue
            } else {
                var t416 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t416
            }
        } else {
            break Loop_loop410
        }
    }
    var t409 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t409
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__166 *_goml_vec_string) int {
    var t419 int = vec_len__Vec_6string(self__166)
    return t419
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t422 string = _goml_runtime_core_bool_to_string(self__66)
    return t422
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t431 int = _goml_runtime_core_string_len(self__38)
    return t431
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t550 bool = index__6 < 0
    var jp548 bool
    if t550 {
        jp548 = true
    } else {
        var t551 bool = index__6 >= length__7
        jp548 = t551
    }
    if jp548 {
        var inline732 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline732
    } else {
        var t435 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t435))
        var t438 bool = first__8 < 128
        if t438 {
            var inline734 int = 1
            var inline735 Option__char = char_from_uint32(first__8)
            switch inline735.(type) {
            case Option__char_None:
                var inline736 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline736
            case Option__char_Some:
                var inline737 rune = inline735.(Option__char_Some)._0
                var inline739 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline737,
                    _2: inline734,
                }
                return inline739
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t442 bool = first__8 < 194
            if t442 {
                var inline741 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline741
            } else {
                var t446 bool = first__8 < 224
                if t446 {
                    var t459 int = length__7 - index__6
                    var t460 bool = t459 < 2
                    if t460 {
                        var inline743 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline743
                    } else {
                        var t448 int = index__6 + 1
                        var t449 uint8
                        var inline757 uint8 = _goml_runtime_core_string_byte_get(value__5, t448)
                        t449 = inline757
                        var second__9 uint32 = uint32(uint8(t449))
                        var t452 bool
                        var inline754 bool = second__9 < 128
                        if inline754 {
                            t452 = true
                        } else {
                            var inline755 bool = second__9 > 191
                            t452 = inline755
                        }
                        if t452 {
                            var inline745 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline745
                        } else {
                            var t454_rhs uint32 = 31
                            var t454 uint32 = first__8 & t454_rhs
                            var t455_rhs int = 6
                            var t455 uint32 = t454 << t455_rhs
                            var t456_rhs uint32 = 63
                            var t456 uint32 = second__9 & t456_rhs
                            var t457 uint32 = t455 | t456
                            var inline747 int = 2
                            var inline748 Option__char = char_from_uint32(t457)
                            switch inline748.(type) {
                            case Option__char_None:
                                var inline749 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline749
                            case Option__char_Some:
                                var inline750 rune = inline748.(Option__char_Some)._0
                                var inline752 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline750,
                                    _2: inline747,
                                }
                                return inline752
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t464 bool = first__8 < 240
                    if t464 {
                        var t497 int = length__7 - index__6
                        var t498 bool = t497 < 3
                        if t498 {
                            var inline759 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline759
                        } else {
                            var t466 int = index__6 + 1
                            var t467 uint8
                            var inline774 uint8 = _goml_runtime_core_string_byte_get(value__5, t466)
                            t467 = inline774
                            var second__10 uint32 = uint32(uint8(t467))
                            var t468 int = index__6 + 2
                            var t469 uint8
                            var inline772 uint8 = _goml_runtime_core_string_byte_get(value__5, t468)
                            t469 = inline772
                            var third__11 uint32 = uint32(uint8(t469))
                            var t495 bool = utf8_invalid_continuation(second__10)
                            var jp490 bool
                            if t495 {
                                jp490 = true
                            } else {
                                var inline761 bool = third__11 < 128
                                if inline761 {
                                    jp490 = true
                                } else {
                                    var inline762 bool = third__11 > 191
                                    jp490 = inline762
                                }
                            }
                            var jp484 bool
                            if jp490 {
                                jp484 = true
                            } else {
                                var t493 bool
                                var inline764 uint32 = 224
                                var inline765 bool = first__8 == inline764
                                t493 = inline765
                                if t493 {
                                    var t494 bool = second__10 < 160
                                    jp484 = t494
                                } else {
                                    jp484 = false
                                }
                            }
                            var jp473 bool
                            if jp484 {
                                jp473 = true
                            } else {
                                var t487 bool
                                var inline767 uint32 = 237
                                var inline768 bool = first__8 == inline767
                                t487 = inline768
                                if t487 {
                                    var t488 bool = second__10 >= 160
                                    jp473 = t488
                                } else {
                                    jp473 = false
                                }
                            }
                            if jp473 {
                                var inline770 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline770
                            } else {
                                var t475_rhs uint32 = 15
                                var t475 uint32 = first__8 & t475_rhs
                                var t476_rhs int = 12
                                var t476 uint32 = t475 << t476_rhs
                                var t477_rhs uint32 = 63
                                var t477 uint32 = second__10 & t477_rhs
                                var t478_rhs int = 6
                                var t478 uint32 = t477 << t478_rhs
                                var t479 uint32 = t476 | t478
                                var t480_rhs uint32 = 63
                                var t480 uint32 = third__11 & t480_rhs
                                var t481 uint32 = t479 | t480
                                var t482 Tuple3_4bool_4char_3int = utf8_valid_decode(t481, 3)
                                return t482
                            }
                        }
                    } else {
                        var t502 bool = first__8 < 245
                        if t502 {
                            var t543 int = length__7 - index__6
                            var t544 bool = t543 < 4
                            if t544 {
                                var t545 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t545
                            } else {
                                var t504 int = index__6 + 1
                                var t505 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t504)
                                var second__12 uint32 = uint32(uint8(t505))
                                var t506 int = index__6 + 2
                                var t507 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t506)
                                var third__13 uint32 = uint32(uint8(t507))
                                var t508 int = index__6 + 3
                                var t509 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t508)
                                var fourth__14 uint32 = uint32(uint8(t509))
                                var t541 bool = utf8_invalid_continuation(second__12)
                                var jp539 bool
                                if t541 {
                                    jp539 = true
                                } else {
                                    var t542 bool = utf8_invalid_continuation(third__13)
                                    jp539 = t542
                                }
                                var jp533 bool
                                if jp539 {
                                    jp533 = true
                                } else {
                                    var t540 bool = utf8_invalid_continuation(fourth__14)
                                    jp533 = t540
                                }
                                var jp527 bool
                                if jp533 {
                                    jp527 = true
                                } else {
                                    var t536 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t536 {
                                        var t537 bool = second__12 < 144
                                        jp527 = t537
                                    } else {
                                        jp527 = false
                                    }
                                }
                                var jp513 bool
                                if jp527 {
                                    jp513 = true
                                } else {
                                    var t530 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t530 {
                                        var t531 bool = second__12 > 143
                                        jp513 = t531
                                    } else {
                                        jp513 = false
                                    }
                                }
                                if jp513 {
                                    var t514 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t514
                                } else {
                                    var t515_rhs uint32 = 7
                                    var t515 uint32 = first__8 & t515_rhs
                                    var t516_rhs int = 18
                                    var t516 uint32 = t515 << t516_rhs
                                    var t517_rhs uint32 = 63
                                    var t517 uint32 = second__12 & t517_rhs
                                    var t518_rhs int = 12
                                    var t518 uint32 = t517 << t518_rhs
                                    var t519 uint32 = t516 | t518
                                    var t520_rhs uint32 = 63
                                    var t520 uint32 = third__13 & t520_rhs
                                    var t521_rhs int = 6
                                    var t521 uint32 = t520 << t521_rhs
                                    var t522 uint32 = t519 | t521
                                    var t523_rhs uint32 = 63
                                    var t523 uint32 = fourth__14 & t523_rhs
                                    var t524 uint32 = t522 | t523
                                    var t525 Tuple3_4bool_4char_3int = utf8_valid_decode(t524, 4)
                                    return t525
                                }
                            }
                        } else {
                            var t546 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t546
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t556 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t556
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t559 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t559
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field796 rune
    var inline778 bool = utf8_valid_scalar(value__0)
    if inline778 {
        var inline779 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline781 rune = inline779._1
        commute_field796 = inline781
        var t565 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field796,
            _2: width__1,
        }
        return t565
    } else {
        var inline776 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline776
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t570 bool = value__3 < 128
    if t570 {
        return true
    } else {
        var t571 bool = value__3 > 191
        return t571
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t574 bool = self__102 == other__103
    return t574
}

func char_from_uint32(value__32 uint32) Option__char {
    var t579 bool
    var inline785 bool = value__32 <= 1114111
    if inline785 {
        var inline786 bool = value__32 >= 55296
        var inline788 bool
        if inline786 {
            var inline790 bool = value__32 <= 57343
            inline788 = inline790
        } else {
            inline788 = false
        }
        var inline789 bool = !inline788
        t579 = inline789
    } else {
        t579 = false
    }
    if t579 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t580 Option__char = Option__char_Some{
            _0: x24,
        }
        return t580
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t585 bool = value__4 <= 1114111
    if t585 {
        var t589 bool = value__4 >= 55296
        var jp587 bool
        if t589 {
            var t590 bool = value__4 <= 57343
            jp587 = t590
        } else {
            jp587 = false
        }
        var t588 bool = !jp587
        return t588
    } else {
        return false
    }
}

func main() {
    main0()
}
