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
    var t189 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t189
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t192 *_goml_vec_uint8
    var inline591 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t192 = inline591
    var t193 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t192,
    }
    return t193
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var commute_field786 _goml_m_std_p_bytes_p_Bytes
    var commute_field788 string
    var inline627 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__0)
    var inline628 bool = inline627._0
    var inline629 *_goml_vec_uint8 = inline627._1
    var inline630 string = inline627._2
    if inline628 {
        var inline634 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline629)
        commute_field786 = inline634
        var inline618 *_goml_vec_uint8 = commute_field786.values
        var inline619 Tuple2_4bool_6string = string_from_utf8(inline618)
        var inline620 bool = inline619._0
        var inline621 string = inline619._1
        if inline620 {
            var inline624 Result__string__string = Result__string__string_Ok{
                _0: inline621,
            }
            return inline624
        } else {
            var inline625 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline625
        }
    } else {
        commute_field788 = inline630
        var t247 Result__string__string = Result__string__string_Err{
            _0: commute_field788,
        }
        return t247
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t261 *_goml_vec_uint8
    var inline652 *_goml_vec_uint8 = data__10.values
    t261 = inline652
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t261)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    if x8 {
        var t264 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t264
    } else {
        var t265 Result__unit__string = Result__unit__string_Err{
            _0: x9,
        }
        return t265
    }
}

func main0() struct{} {
    var inline706 string = "goml-std-test.txt"
    var inline707 string = "std-ok"
    var inline708 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline707)
    _goml_m_std_p_fs_p_write__bytes(inline706, inline708)
    var t357 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t358 string
    switch t357.(type) {
    case Result__string__string_Ok:
        var inline700 string = t357.(Result__string__string_Ok)._0
        t358 = inline700
    case Result__string__string_Err:
        var inline702 string = t357.(Result__string__string_Err)._0
        var inline704 string = "err " + inline702
        t358 = inline704
    default:
        panic("non-exhaustive match")
    }
    var inline697 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t358)
    _goml_runtime_std_io_println(inline697)
    var t359 bool
    var inline694 string = "goml-std-test.txt"
    var inline695 bool = _goml_runtime_std_fs_file_exists(inline694)
    t359 = inline695
    var t360 string
    var inline692 string = _goml_runtime_core_bool_to_string(t359)
    t360 = inline692
    var inline689 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t360)
    _goml_runtime_std_io_println(inline689)
    var t361 _goml_m_Result____Vec_l_string_r_____string
    var inline678 string = "."
    var inline679 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(inline678)
    var inline680 bool = inline679._0
    var inline681 *_goml_vec_string = inline679._1
    var inline682 string = inline679._2
    if inline680 {
        var inline686 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline681,
        }
        t361 = inline686
    } else {
        var inline687 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline682,
        }
        t361 = inline687
    }
    var t362 string
    switch t361.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline669 *_goml_vec_string = t361.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline671 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline669)
        var inline672 bool = inline671 > 0
        var inline673 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline672)
        t362 = inline673
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline674 string = t361.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline676 string = "err " + inline674
        t362 = inline676
    default:
        panic("non-exhaustive match")
    }
    var inline666 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t362)
    _goml_runtime_std_io_println(inline666)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop405:
    for {
        var t406 int
        var inline724 int = _goml_runtime_core_string_len(x12)
        t406 = inline724
        var t407 bool = index__26 < t406
        if t407 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t409 int = compound_old17 + x16
                index__26 = t409
                continue
            } else {
                var t411 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t411
            }
        } else {
            break Loop_loop405
        }
    }
    var t404 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t404
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__187 *_goml_vec_string) int {
    var t414 int = vec_len__Vec_6string(self__187)
    return t414
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t417 string = _goml_runtime_core_bool_to_string(self__66)
    return t417
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t426 int = _goml_runtime_core_string_len(self__38)
    return t426
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t545 bool = index__6 < 0
    var jp543 bool
    if t545 {
        jp543 = true
    } else {
        var t546 bool = index__6 >= length__7
        jp543 = t546
    }
    if jp543 {
        var inline727 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline727
    } else {
        var t430 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t430))
        var t433 bool = first__8 < 128
        if t433 {
            var inline729 int = 1
            var inline730 Option__char = char_from_uint32(first__8)
            switch inline730.(type) {
            case Option__char_None:
                var inline731 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline731
            case Option__char_Some:
                var inline732 rune = inline730.(Option__char_Some)._0
                var inline734 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline732,
                    _2: inline729,
                }
                return inline734
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t437 bool = first__8 < 194
            if t437 {
                var inline736 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline736
            } else {
                var t441 bool = first__8 < 224
                if t441 {
                    var t454 int = length__7 - index__6
                    var t455 bool = t454 < 2
                    if t455 {
                        var inline738 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline738
                    } else {
                        var t443 int = index__6 + 1
                        var t444 uint8
                        var inline752 uint8 = _goml_runtime_core_string_byte_get(value__5, t443)
                        t444 = inline752
                        var second__9 uint32 = uint32(uint8(t444))
                        var t447 bool
                        var inline749 bool = second__9 < 128
                        if inline749 {
                            t447 = true
                        } else {
                            var inline750 bool = second__9 > 191
                            t447 = inline750
                        }
                        if t447 {
                            var inline740 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline740
                        } else {
                            var t449_rhs uint32 = 31
                            var t449 uint32 = first__8 & t449_rhs
                            var t450_rhs int = 6
                            var t450 uint32 = t449 << t450_rhs
                            var t451_rhs uint32 = 63
                            var t451 uint32 = second__9 & t451_rhs
                            var t452 uint32 = t450 | t451
                            var inline742 int = 2
                            var inline743 Option__char = char_from_uint32(t452)
                            switch inline743.(type) {
                            case Option__char_None:
                                var inline744 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline744
                            case Option__char_Some:
                                var inline745 rune = inline743.(Option__char_Some)._0
                                var inline747 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline745,
                                    _2: inline742,
                                }
                                return inline747
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t459 bool = first__8 < 240
                    if t459 {
                        var t492 int = length__7 - index__6
                        var t493 bool = t492 < 3
                        if t493 {
                            var inline754 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline754
                        } else {
                            var t461 int = index__6 + 1
                            var t462 uint8
                            var inline769 uint8 = _goml_runtime_core_string_byte_get(value__5, t461)
                            t462 = inline769
                            var second__10 uint32 = uint32(uint8(t462))
                            var t463 int = index__6 + 2
                            var t464 uint8
                            var inline767 uint8 = _goml_runtime_core_string_byte_get(value__5, t463)
                            t464 = inline767
                            var third__11 uint32 = uint32(uint8(t464))
                            var t490 bool = utf8_invalid_continuation(second__10)
                            var jp485 bool
                            if t490 {
                                jp485 = true
                            } else {
                                var inline756 bool = third__11 < 128
                                if inline756 {
                                    jp485 = true
                                } else {
                                    var inline757 bool = third__11 > 191
                                    jp485 = inline757
                                }
                            }
                            var jp479 bool
                            if jp485 {
                                jp479 = true
                            } else {
                                var t488 bool
                                var inline759 uint32 = 224
                                var inline760 bool = first__8 == inline759
                                t488 = inline760
                                if t488 {
                                    var t489 bool = second__10 < 160
                                    jp479 = t489
                                } else {
                                    jp479 = false
                                }
                            }
                            var jp468 bool
                            if jp479 {
                                jp468 = true
                            } else {
                                var t482 bool
                                var inline762 uint32 = 237
                                var inline763 bool = first__8 == inline762
                                t482 = inline763
                                if t482 {
                                    var t483 bool = second__10 >= 160
                                    jp468 = t483
                                } else {
                                    jp468 = false
                                }
                            }
                            if jp468 {
                                var inline765 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline765
                            } else {
                                var t470_rhs uint32 = 15
                                var t470 uint32 = first__8 & t470_rhs
                                var t471_rhs int = 12
                                var t471 uint32 = t470 << t471_rhs
                                var t472_rhs uint32 = 63
                                var t472 uint32 = second__10 & t472_rhs
                                var t473_rhs int = 6
                                var t473 uint32 = t472 << t473_rhs
                                var t474 uint32 = t471 | t473
                                var t475_rhs uint32 = 63
                                var t475 uint32 = third__11 & t475_rhs
                                var t476 uint32 = t474 | t475
                                var t477 Tuple3_4bool_4char_3int = utf8_valid_decode(t476, 3)
                                return t477
                            }
                        }
                    } else {
                        var t497 bool = first__8 < 245
                        if t497 {
                            var t538 int = length__7 - index__6
                            var t539 bool = t538 < 4
                            if t539 {
                                var t540 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t540
                            } else {
                                var t499 int = index__6 + 1
                                var t500 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t499)
                                var second__12 uint32 = uint32(uint8(t500))
                                var t501 int = index__6 + 2
                                var t502 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t501)
                                var third__13 uint32 = uint32(uint8(t502))
                                var t503 int = index__6 + 3
                                var t504 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t503)
                                var fourth__14 uint32 = uint32(uint8(t504))
                                var t536 bool = utf8_invalid_continuation(second__12)
                                var jp534 bool
                                if t536 {
                                    jp534 = true
                                } else {
                                    var t537 bool = utf8_invalid_continuation(third__13)
                                    jp534 = t537
                                }
                                var jp528 bool
                                if jp534 {
                                    jp528 = true
                                } else {
                                    var t535 bool = utf8_invalid_continuation(fourth__14)
                                    jp528 = t535
                                }
                                var jp522 bool
                                if jp528 {
                                    jp522 = true
                                } else {
                                    var t531 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t531 {
                                        var t532 bool = second__12 < 144
                                        jp522 = t532
                                    } else {
                                        jp522 = false
                                    }
                                }
                                var jp508 bool
                                if jp522 {
                                    jp508 = true
                                } else {
                                    var t525 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t525 {
                                        var t526 bool = second__12 > 143
                                        jp508 = t526
                                    } else {
                                        jp508 = false
                                    }
                                }
                                if jp508 {
                                    var t509 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t509
                                } else {
                                    var t510_rhs uint32 = 7
                                    var t510 uint32 = first__8 & t510_rhs
                                    var t511_rhs int = 18
                                    var t511 uint32 = t510 << t511_rhs
                                    var t512_rhs uint32 = 63
                                    var t512 uint32 = second__12 & t512_rhs
                                    var t513_rhs int = 12
                                    var t513 uint32 = t512 << t513_rhs
                                    var t514 uint32 = t511 | t513
                                    var t515_rhs uint32 = 63
                                    var t515 uint32 = third__13 & t515_rhs
                                    var t516_rhs int = 6
                                    var t516 uint32 = t515 << t516_rhs
                                    var t517 uint32 = t514 | t516
                                    var t518_rhs uint32 = 63
                                    var t518 uint32 = fourth__14 & t518_rhs
                                    var t519 uint32 = t517 | t518
                                    var t520 Tuple3_4bool_4char_3int = utf8_valid_decode(t519, 4)
                                    return t520
                                }
                            }
                        } else {
                            var t541 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t541
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
    var t551 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t551
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t554 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t554
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field791 rune
    var inline773 bool = utf8_valid_scalar(value__0)
    if inline773 {
        var inline774 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline776 rune = inline774._1
        commute_field791 = inline776
        var t560 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field791,
            _2: width__1,
        }
        return t560
    } else {
        var inline771 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline771
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t565 bool = value__3 < 128
    if t565 {
        return true
    } else {
        var t566 bool = value__3 > 191
        return t566
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t569 bool = self__117 == other__118
    return t569
}

func char_from_uint32(value__32 uint32) Option__char {
    var t574 bool
    var inline780 bool = value__32 <= 1114111
    if inline780 {
        var inline781 bool = value__32 >= 55296
        var inline783 bool
        if inline781 {
            var inline785 bool = value__32 <= 57343
            inline783 = inline785
        } else {
            inline783 = false
        }
        var inline784 bool = !inline783
        t574 = inline784
    } else {
        t574 = false
    }
    if t574 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t575 Option__char = Option__char_Some{
            _0: x24,
        }
        return t575
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t580 bool = value__4 <= 1114111
    if t580 {
        var t584 bool = value__4 >= 55296
        var jp582 bool
        if t584 {
            var t585 bool = value__4 <= 57343
            jp582 = t585
        } else {
            jp582 = false
        }
        var t583 bool = !jp582
        return t583
    } else {
        return false
    }
}

func main() {
    main0()
}
