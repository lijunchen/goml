package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
)

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
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

func _goml_runtime_std_fs_create_dir_all(path string) Tuple2_4bool_6string {
    var err error = _goml_os.MkdirAll(path, 0755)
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

func _goml_runtime_std_io_println(value string) struct{} {
    _goml_fmt.Println(value)
    return struct{}{}
}

func _goml_runtime_std_io_eprint(value string) struct{} {
    _goml_fmt.Fprint(_goml_os.Stderr, value)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_string struct {
    items []string
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

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t157 *_goml_vec_uint8
    var inline586 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t157 = inline586
    var t158 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t157,
    }
    return t158
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t202 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t202)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t205 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t205
    } else {
        var t206 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t206
    }
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    if x4 {
        var t221 _goml_m_std_p_bytes_p_Bytes
        var inline645 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x5,
        }
        t221 = inline645
        var t222 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t221,
        }
        return t222
    } else {
        var t223 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x6,
        }
        return t223
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t226 *_goml_vec_uint8
    var inline647 *_goml_vec_uint8 = data__10.values
    t226 = inline647
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t226)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    if x8 {
        var t229 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t229
    } else {
        var t230 Result__unit__string = Result__unit__string_Err{
            _0: x9,
        }
        return t230
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__13 string) Result__unit__string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__13)
    var x11 bool = mtmp10._0
    var x12 string = mtmp10._1
    if x11 {
        var t235 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t235
    } else {
        var t236 Result__unit__string = Result__unit__string_Err{
            _0: x12,
        }
        return t236
    }
}

func main0() struct{} {
    var t319 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t319)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline696 string = ""
    var inline697 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline696)
    var inline698 string = inline697 + "\n"
    _goml_runtime_std_io_eprint(inline698)
    var t320 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t321 string
    switch t320.(type) {
    case Result__unit__string_Ok:
        t321 = "ok"
    case Result__unit__string_Err:
        var inline692 string = t320.(Result__unit__string_Err)._0
        var inline694 string = "err " + inline692
        t321 = inline694
    default:
        panic("non-exhaustive match")
    }
    var inline688 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t321)
    _goml_runtime_std_io_println(inline688)
    var t322 Result__unit__string
    var inline683 string = "goml-self-host/nested/output.txt"
    var inline684 string = "boot"
    var inline685 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline684)
    var inline686 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline683, inline685)
    t322 = inline686
    var t323 string
    switch t322.(type) {
    case Result__unit__string_Ok:
        t323 = "ok"
    case Result__unit__string_Err:
        var inline679 string = t322.(Result__unit__string_Err)._0
        var inline681 string = "err " + inline679
        t323 = inline681
    default:
        panic("non-exhaustive match")
    }
    var inline675 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t323)
    _goml_runtime_std_io_println(inline675)
    var t324 Result__string__string
    var inline666 string = "goml-self-host/nested/output.txt"
    var inline667 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline666)
    switch inline667.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline668 _goml_m_std_p_bytes_p_Bytes = inline667.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline670 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline668)
        t324 = inline670
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline671 string = inline667.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline673 Result__string__string = Result__string__string_Err{
            _0: inline671,
        }
        t324 = inline673
    default:
        panic("non-exhaustive match")
    }
    var t325 string
    switch t324.(type) {
    case Result__string__string_Ok:
        var inline660 string = t324.(Result__string__string_Ok)._0
        t325 = inline660
    case Result__string__string_Err:
        var inline662 string = t324.(Result__string__string_Err)._0
        var inline664 string = "err " + inline662
        t325 = inline664
    default:
        panic("non-exhaustive match")
    }
    var inline657 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t325)
    _goml_runtime_std_io_println(inline657)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop369:
    for {
        var t370 int
        var inline714 int = _goml_runtime_core_string_len(x12)
        t370 = inline714
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

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t377 string
    t377 = value__1
    _goml_runtime_std_io_println(t377)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t387 bool = string_is_char_boundary(value__21, start__22)
    var jp384 bool
    if t387 {
        var t388 bool = string_is_char_boundary(value__21, end__23)
        jp384 = t388
    } else {
        jp384 = false
    }
    if jp384 {
        var t385 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t385
    } else {
        var t386 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t386
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t390 string
    t390 = value__2
    _goml_runtime_std_io_eprint(t390)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t401 int = _goml_runtime_core_string_len(self__38)
    return t401
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t520 bool = index__6 < 0
    var jp518 bool
    if t520 {
        jp518 = true
    } else {
        var t521 bool = index__6 >= length__7
        jp518 = t521
    }
    if jp518 {
        var inline719 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline719
    } else {
        var t405 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t405))
        var t408 bool = first__8 < 128
        if t408 {
            var inline721 int = 1
            var inline722 Option__char = char_from_uint32(first__8)
            switch inline722.(type) {
            case Option__char_None:
                var inline723 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline723
            case Option__char_Some:
                var inline724 rune = inline722.(Option__char_Some)._0
                var inline726 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline724,
                    _2: inline721,
                }
                return inline726
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t412 bool = first__8 < 194
            if t412 {
                var inline728 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline728
            } else {
                var t416 bool = first__8 < 224
                if t416 {
                    var t429 int = length__7 - index__6
                    var t430 bool = t429 < 2
                    if t430 {
                        var inline730 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline730
                    } else {
                        var t418 int = index__6 + 1
                        var t419 uint8
                        var inline744 uint8 = _goml_runtime_core_string_byte_get(value__5, t418)
                        t419 = inline744
                        var second__9 uint32 = uint32(uint8(t419))
                        var t422 bool
                        var inline741 bool = second__9 < 128
                        if inline741 {
                            t422 = true
                        } else {
                            var inline742 bool = second__9 > 191
                            t422 = inline742
                        }
                        if t422 {
                            var inline732 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline732
                        } else {
                            var t424_rhs uint32 = 31
                            var t424 uint32 = first__8 & t424_rhs
                            var t425_rhs int = 6
                            var t425 uint32 = t424 << t425_rhs
                            var t426_rhs uint32 = 63
                            var t426 uint32 = second__9 & t426_rhs
                            var t427 uint32 = t425 | t426
                            var inline734 int = 2
                            var inline735 Option__char = char_from_uint32(t427)
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
                        }
                    }
                } else {
                    var t434 bool = first__8 < 240
                    if t434 {
                        var t467 int = length__7 - index__6
                        var t468 bool = t467 < 3
                        if t468 {
                            var inline746 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline746
                        } else {
                            var t436 int = index__6 + 1
                            var t437 uint8
                            var inline761 uint8 = _goml_runtime_core_string_byte_get(value__5, t436)
                            t437 = inline761
                            var second__10 uint32 = uint32(uint8(t437))
                            var t438 int = index__6 + 2
                            var t439 uint8
                            var inline759 uint8 = _goml_runtime_core_string_byte_get(value__5, t438)
                            t439 = inline759
                            var third__11 uint32 = uint32(uint8(t439))
                            var t465 bool = utf8_invalid_continuation(second__10)
                            var jp460 bool
                            if t465 {
                                jp460 = true
                            } else {
                                var inline748 bool = third__11 < 128
                                if inline748 {
                                    jp460 = true
                                } else {
                                    var inline749 bool = third__11 > 191
                                    jp460 = inline749
                                }
                            }
                            var jp454 bool
                            if jp460 {
                                jp454 = true
                            } else {
                                var t463 bool
                                var inline751 uint32 = 224
                                var inline752 bool = first__8 == inline751
                                t463 = inline752
                                if t463 {
                                    var t464 bool = second__10 < 160
                                    jp454 = t464
                                } else {
                                    jp454 = false
                                }
                            }
                            var jp443 bool
                            if jp454 {
                                jp443 = true
                            } else {
                                var t457 bool
                                var inline754 uint32 = 237
                                var inline755 bool = first__8 == inline754
                                t457 = inline755
                                if t457 {
                                    var t458 bool = second__10 >= 160
                                    jp443 = t458
                                } else {
                                    jp443 = false
                                }
                            }
                            if jp443 {
                                var inline757 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline757
                            } else {
                                var t445_rhs uint32 = 15
                                var t445 uint32 = first__8 & t445_rhs
                                var t446_rhs int = 12
                                var t446 uint32 = t445 << t446_rhs
                                var t447_rhs uint32 = 63
                                var t447 uint32 = second__10 & t447_rhs
                                var t448_rhs int = 6
                                var t448 uint32 = t447 << t448_rhs
                                var t449 uint32 = t446 | t448
                                var t450_rhs uint32 = 63
                                var t450 uint32 = third__11 & t450_rhs
                                var t451 uint32 = t449 | t450
                                var t452 Tuple3_4bool_4char_3int = utf8_valid_decode(t451, 3)
                                return t452
                            }
                        }
                    } else {
                        var t472 bool = first__8 < 245
                        if t472 {
                            var t513 int = length__7 - index__6
                            var t514 bool = t513 < 4
                            if t514 {
                                var t515 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t515
                            } else {
                                var t474 int = index__6 + 1
                                var t475 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t474)
                                var second__12 uint32 = uint32(uint8(t475))
                                var t476 int = index__6 + 2
                                var t477 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t476)
                                var third__13 uint32 = uint32(uint8(t477))
                                var t478 int = index__6 + 3
                                var t479 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t478)
                                var fourth__14 uint32 = uint32(uint8(t479))
                                var t511 bool = utf8_invalid_continuation(second__12)
                                var jp509 bool
                                if t511 {
                                    jp509 = true
                                } else {
                                    var t512 bool = utf8_invalid_continuation(third__13)
                                    jp509 = t512
                                }
                                var jp503 bool
                                if jp509 {
                                    jp503 = true
                                } else {
                                    var t510 bool = utf8_invalid_continuation(fourth__14)
                                    jp503 = t510
                                }
                                var jp497 bool
                                if jp503 {
                                    jp497 = true
                                } else {
                                    var t506 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t506 {
                                        var t507 bool = second__12 < 144
                                        jp497 = t507
                                    } else {
                                        jp497 = false
                                    }
                                }
                                var jp483 bool
                                if jp497 {
                                    jp483 = true
                                } else {
                                    var t500 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t500 {
                                        var t501 bool = second__12 > 143
                                        jp483 = t501
                                    } else {
                                        jp483 = false
                                    }
                                }
                                if jp483 {
                                    var t484 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t484
                                } else {
                                    var t485_rhs uint32 = 7
                                    var t485 uint32 = first__8 & t485_rhs
                                    var t486_rhs int = 18
                                    var t486 uint32 = t485 << t486_rhs
                                    var t487_rhs uint32 = 63
                                    var t487 uint32 = second__12 & t487_rhs
                                    var t488_rhs int = 12
                                    var t488 uint32 = t487 << t488_rhs
                                    var t489 uint32 = t486 | t488
                                    var t490_rhs uint32 = 63
                                    var t490 uint32 = third__13 & t490_rhs
                                    var t491_rhs int = 6
                                    var t491 uint32 = t490 << t491_rhs
                                    var t492 uint32 = t489 | t491
                                    var t493_rhs uint32 = 63
                                    var t493 uint32 = fourth__14 & t493_rhs
                                    var t494 uint32 = t492 | t493
                                    var t495 Tuple3_4bool_4char_3int = utf8_valid_decode(t494, 4)
                                    return t495
                                }
                            }
                        } else {
                            var t516 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t516
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

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t538 bool = index__16 < 0
    var jp529 bool
    if t538 {
        jp529 = true
    } else {
        var t539 int
        var inline763 int = _goml_runtime_core_string_len(value__15)
        t539 = inline763
        var t540 bool = index__16 > t539
        jp529 = t540
    }
    if jp529 {
        return false
    } else {
        var t532 int
        var inline772 int = _goml_runtime_core_string_len(value__15)
        t532 = inline772
        var t533 bool
        var inline770 bool = index__16 == t532
        t533 = inline770
        if t533 {
            return true
        } else {
            var t534 uint8
            var inline768 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t534 = inline768
            var t535_rhs uint8 = 192
            var t535 uint8 = t534 & t535_rhs
            var t536 bool
            var inline765 uint8 = 128
            var inline766 bool = t535 == inline765
            t536 = inline766
            var t537 bool = !t536
            return t537
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t543 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t543
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t546 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t546
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field794 rune
    var inline776 bool = utf8_valid_scalar(value__0)
    if inline776 {
        var inline777 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline779 rune = inline777._1
        commute_field794 = inline779
        var t552 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field794,
            _2: width__1,
        }
        return t552
    } else {
        var inline774 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline774
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t557 bool = value__3 < 128
    if t557 {
        return true
    } else {
        var t558 bool = value__3 > 191
        return t558
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t561 bool = self__117 == other__118
    return t561
}

func char_from_uint32(value__32 uint32) Option__char {
    var t569 bool
    var inline783 bool = value__32 <= 1114111
    if inline783 {
        var inline784 bool = value__32 >= 55296
        var inline786 bool
        if inline784 {
            var inline788 bool = value__32 <= 57343
            inline786 = inline788
        } else {
            inline786 = false
        }
        var inline787 bool = !inline786
        t569 = inline787
    } else {
        t569 = false
    }
    if t569 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t570 Option__char = Option__char_Some{
            _0: x24,
        }
        return t570
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t575 bool = value__4 <= 1114111
    if t575 {
        var t579 bool = value__4 >= 55296
        var jp577 bool
        if t579 {
            var t580 bool = value__4 <= 57343
            jp577 = t580
        } else {
            jp577 = false
        }
        var t578 bool = !jp577
        return t578
    } else {
        return false
    }
}

func main() {
    main0()
}
