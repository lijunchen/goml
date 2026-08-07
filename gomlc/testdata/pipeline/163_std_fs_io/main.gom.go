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
    var t153 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t153
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t156 *_goml_vec_uint8
    var inline555 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t156 = inline555
    var t157 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t156,
    }
    return t157
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var commute_field750 _goml_m_std_p_bytes_p_Bytes
    var commute_field752 string
    var inline591 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__0)
    var inline592 bool = inline591._0
    var inline593 *_goml_vec_uint8 = inline591._1
    var inline594 string = inline591._2
    if inline592 {
        var inline598 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline593)
        commute_field750 = inline598
        var inline582 *_goml_vec_uint8 = commute_field750.values
        var inline583 Tuple2_4bool_6string = string_from_utf8(inline582)
        var inline584 bool = inline583._0
        var inline585 string = inline583._1
        if inline584 {
            var inline588 Result__string__string = Result__string__string_Ok{
                _0: inline585,
            }
            return inline588
        } else {
            var inline589 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline589
        }
    } else {
        commute_field752 = inline594
        var t211 Result__string__string = Result__string__string_Err{
            _0: commute_field752,
        }
        return t211
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t225 *_goml_vec_uint8
    var inline616 *_goml_vec_uint8 = data__10.values
    t225 = inline616
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t225)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    if x8 {
        var t228 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t228
    } else {
        var t229 Result__unit__string = Result__unit__string_Err{
            _0: x9,
        }
        return t229
    }
}

func main0() struct{} {
    var inline670 string = "goml-std-test.txt"
    var inline671 string = "std-ok"
    var inline672 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline671)
    _goml_m_std_p_fs_p_write__bytes(inline670, inline672)
    var t321 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t322 string
    switch t321.(type) {
    case Result__string__string_Ok:
        var inline664 string = t321.(Result__string__string_Ok)._0
        t322 = inline664
    case Result__string__string_Err:
        var inline666 string = t321.(Result__string__string_Err)._0
        var inline668 string = "err " + inline666
        t322 = inline668
    default:
        panic("non-exhaustive match")
    }
    var inline661 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t322)
    _goml_runtime_std_io_println(inline661)
    var t323 bool
    var inline658 string = "goml-std-test.txt"
    var inline659 bool = _goml_runtime_std_fs_file_exists(inline658)
    t323 = inline659
    var t324 string
    var inline656 string = _goml_runtime_core_bool_to_string(t323)
    t324 = inline656
    var inline653 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t324)
    _goml_runtime_std_io_println(inline653)
    var t325 _goml_m_Result____Vec_l_string_r_____string
    var inline642 string = "."
    var inline643 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(inline642)
    var inline644 bool = inline643._0
    var inline645 *_goml_vec_string = inline643._1
    var inline646 string = inline643._2
    if inline644 {
        var inline650 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline645,
        }
        t325 = inline650
    } else {
        var inline651 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline646,
        }
        t325 = inline651
    }
    var t326 string
    switch t325.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline633 *_goml_vec_string = t325.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline635 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline633)
        var inline636 bool = inline635 > 0
        var inline637 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline636)
        t326 = inline637
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline638 string = t325.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline640 string = "err " + inline638
        t326 = inline640
    default:
        panic("non-exhaustive match")
    }
    var inline630 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t326)
    _goml_runtime_std_io_println(inline630)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop369:
    for {
        var t370 int
        var inline688 int = _goml_runtime_core_string_len(x12)
        t370 = inline688
        var t371 bool = index__26 < t370
        if t371 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t373 int = compound_old17 + x16
                index__26 = t373
                continue
            } else {
                var t375 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t375
            }
        } else {
            break Loop_loop369
        }
    }
    var t368 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t368
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__162 *_goml_vec_string) int {
    var t378 int = vec_len__Vec_6string(self__162)
    return t378
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t381 string = _goml_runtime_core_bool_to_string(self__66)
    return t381
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t390 int = _goml_runtime_core_string_len(self__38)
    return t390
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t509 bool = index__6 < 0
    var jp507 bool
    if t509 {
        jp507 = true
    } else {
        var t510 bool = index__6 >= length__7
        jp507 = t510
    }
    if jp507 {
        var inline691 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline691
    } else {
        var t394 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t394))
        var t397 bool = first__8 < 128
        if t397 {
            var inline693 int = 1
            var inline694 Option__char = char_from_uint32(first__8)
            switch inline694.(type) {
            case Option__char_None:
                var inline695 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline695
            case Option__char_Some:
                var inline696 rune = inline694.(Option__char_Some)._0
                var inline698 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline696,
                    _2: inline693,
                }
                return inline698
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t401 bool = first__8 < 194
            if t401 {
                var inline700 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline700
            } else {
                var t405 bool = first__8 < 224
                if t405 {
                    var t418 int = length__7 - index__6
                    var t419 bool = t418 < 2
                    if t419 {
                        var inline702 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline702
                    } else {
                        var t407 int = index__6 + 1
                        var t408 uint8
                        var inline716 uint8 = _goml_runtime_core_string_byte_get(value__5, t407)
                        t408 = inline716
                        var second__9 uint32 = uint32(uint8(t408))
                        var t411 bool
                        var inline713 bool = second__9 < 128
                        if inline713 {
                            t411 = true
                        } else {
                            var inline714 bool = second__9 > 191
                            t411 = inline714
                        }
                        if t411 {
                            var inline704 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline704
                        } else {
                            var t413_rhs uint32 = 31
                            var t413 uint32 = first__8 & t413_rhs
                            var t414_rhs int = 6
                            var t414 uint32 = t413 << t414_rhs
                            var t415_rhs uint32 = 63
                            var t415 uint32 = second__9 & t415_rhs
                            var t416 uint32 = t414 | t415
                            var inline706 int = 2
                            var inline707 Option__char = char_from_uint32(t416)
                            switch inline707.(type) {
                            case Option__char_None:
                                var inline708 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline708
                            case Option__char_Some:
                                var inline709 rune = inline707.(Option__char_Some)._0
                                var inline711 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline709,
                                    _2: inline706,
                                }
                                return inline711
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t423 bool = first__8 < 240
                    if t423 {
                        var t456 int = length__7 - index__6
                        var t457 bool = t456 < 3
                        if t457 {
                            var inline718 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline718
                        } else {
                            var t425 int = index__6 + 1
                            var t426 uint8
                            var inline733 uint8 = _goml_runtime_core_string_byte_get(value__5, t425)
                            t426 = inline733
                            var second__10 uint32 = uint32(uint8(t426))
                            var t427 int = index__6 + 2
                            var t428 uint8
                            var inline731 uint8 = _goml_runtime_core_string_byte_get(value__5, t427)
                            t428 = inline731
                            var third__11 uint32 = uint32(uint8(t428))
                            var t454 bool = utf8_invalid_continuation(second__10)
                            var jp449 bool
                            if t454 {
                                jp449 = true
                            } else {
                                var inline720 bool = third__11 < 128
                                if inline720 {
                                    jp449 = true
                                } else {
                                    var inline721 bool = third__11 > 191
                                    jp449 = inline721
                                }
                            }
                            var jp443 bool
                            if jp449 {
                                jp443 = true
                            } else {
                                var t452 bool
                                var inline723 uint32 = 224
                                var inline724 bool = first__8 == inline723
                                t452 = inline724
                                if t452 {
                                    var t453 bool = second__10 < 160
                                    jp443 = t453
                                } else {
                                    jp443 = false
                                }
                            }
                            var jp432 bool
                            if jp443 {
                                jp432 = true
                            } else {
                                var t446 bool
                                var inline726 uint32 = 237
                                var inline727 bool = first__8 == inline726
                                t446 = inline727
                                if t446 {
                                    var t447 bool = second__10 >= 160
                                    jp432 = t447
                                } else {
                                    jp432 = false
                                }
                            }
                            if jp432 {
                                var inline729 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline729
                            } else {
                                var t434_rhs uint32 = 15
                                var t434 uint32 = first__8 & t434_rhs
                                var t435_rhs int = 12
                                var t435 uint32 = t434 << t435_rhs
                                var t436_rhs uint32 = 63
                                var t436 uint32 = second__10 & t436_rhs
                                var t437_rhs int = 6
                                var t437 uint32 = t436 << t437_rhs
                                var t438 uint32 = t435 | t437
                                var t439_rhs uint32 = 63
                                var t439 uint32 = third__11 & t439_rhs
                                var t440 uint32 = t438 | t439
                                var t441 Tuple3_4bool_4char_3int = utf8_valid_decode(t440, 3)
                                return t441
                            }
                        }
                    } else {
                        var t461 bool = first__8 < 245
                        if t461 {
                            var t502 int = length__7 - index__6
                            var t503 bool = t502 < 4
                            if t503 {
                                var t504 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t504
                            } else {
                                var t463 int = index__6 + 1
                                var t464 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t463)
                                var second__12 uint32 = uint32(uint8(t464))
                                var t465 int = index__6 + 2
                                var t466 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t465)
                                var third__13 uint32 = uint32(uint8(t466))
                                var t467 int = index__6 + 3
                                var t468 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t467)
                                var fourth__14 uint32 = uint32(uint8(t468))
                                var t500 bool = utf8_invalid_continuation(second__12)
                                var jp498 bool
                                if t500 {
                                    jp498 = true
                                } else {
                                    var t501 bool = utf8_invalid_continuation(third__13)
                                    jp498 = t501
                                }
                                var jp492 bool
                                if jp498 {
                                    jp492 = true
                                } else {
                                    var t499 bool = utf8_invalid_continuation(fourth__14)
                                    jp492 = t499
                                }
                                var jp486 bool
                                if jp492 {
                                    jp486 = true
                                } else {
                                    var t495 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t495 {
                                        var t496 bool = second__12 < 144
                                        jp486 = t496
                                    } else {
                                        jp486 = false
                                    }
                                }
                                var jp472 bool
                                if jp486 {
                                    jp472 = true
                                } else {
                                    var t489 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t489 {
                                        var t490 bool = second__12 > 143
                                        jp472 = t490
                                    } else {
                                        jp472 = false
                                    }
                                }
                                if jp472 {
                                    var t473 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t473
                                } else {
                                    var t474_rhs uint32 = 7
                                    var t474 uint32 = first__8 & t474_rhs
                                    var t475_rhs int = 18
                                    var t475 uint32 = t474 << t475_rhs
                                    var t476_rhs uint32 = 63
                                    var t476 uint32 = second__12 & t476_rhs
                                    var t477_rhs int = 12
                                    var t477 uint32 = t476 << t477_rhs
                                    var t478 uint32 = t475 | t477
                                    var t479_rhs uint32 = 63
                                    var t479 uint32 = third__13 & t479_rhs
                                    var t480_rhs int = 6
                                    var t480 uint32 = t479 << t480_rhs
                                    var t481 uint32 = t478 | t480
                                    var t482_rhs uint32 = 63
                                    var t482 uint32 = fourth__14 & t482_rhs
                                    var t483 uint32 = t481 | t482
                                    var t484 Tuple3_4bool_4char_3int = utf8_valid_decode(t483, 4)
                                    return t484
                                }
                            }
                        } else {
                            var t505 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t505
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
    var t515 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t515
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t518 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t518
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field755 rune
    var inline737 bool = utf8_valid_scalar(value__0)
    if inline737 {
        var inline738 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline740 rune = inline738._1
        commute_field755 = inline740
        var t524 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field755,
            _2: width__1,
        }
        return t524
    } else {
        var inline735 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline735
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t529 bool = value__3 < 128
    if t529 {
        return true
    } else {
        var t530 bool = value__3 > 191
        return t530
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t533 bool = self__117 == other__118
    return t533
}

func char_from_uint32(value__32 uint32) Option__char {
    var t538 bool
    var inline744 bool = value__32 <= 1114111
    if inline744 {
        var inline745 bool = value__32 >= 55296
        var inline747 bool
        if inline745 {
            var inline749 bool = value__32 <= 57343
            inline747 = inline749
        } else {
            inline747 = false
        }
        var inline748 bool = !inline747
        t538 = inline748
    } else {
        t538 = false
    }
    if t538 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t539 Option__char = Option__char_Some{
            _0: x24,
        }
        return t539
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t544 bool = value__4 <= 1114111
    if t544 {
        var t548 bool = value__4 >= 55296
        var jp546 bool
        if t548 {
            var t549 bool = value__4 <= 57343
            jp546 = t549
        } else {
            jp546 = false
        }
        var t547 bool = !jp546
        return t547
    } else {
        return false
    }
}

func main() {
    main0()
}
