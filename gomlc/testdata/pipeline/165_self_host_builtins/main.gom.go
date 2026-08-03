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
    var t198 *_goml_vec_uint8
    var inline627 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t198 = inline627
    var t199 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t198,
    }
    return t199
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t243 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t243)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t246 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t246
    } else {
        var t247 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t247
    }
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    if x4 {
        var t262 _goml_m_std_p_bytes_p_Bytes
        var inline686 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x5,
        }
        t262 = inline686
        var t263 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t262,
        }
        return t263
    } else {
        var t264 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x6,
        }
        return t264
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t267 *_goml_vec_uint8
    var inline688 *_goml_vec_uint8 = data__10.values
    t267 = inline688
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t267)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    if x8 {
        var t270 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t270
    } else {
        var t271 Result__unit__string = Result__unit__string_Err{
            _0: x9,
        }
        return t271
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__13 string) Result__unit__string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__13)
    var x11 bool = mtmp10._0
    var x12 string = mtmp10._1
    if x11 {
        var t276 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t276
    } else {
        var t277 Result__unit__string = Result__unit__string_Err{
            _0: x12,
        }
        return t277
    }
}

func main0() struct{} {
    var t360 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t360)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline737 string = ""
    var inline738 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline737)
    var inline739 string = inline738 + "\n"
    _goml_runtime_std_io_eprint(inline739)
    var t361 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t362 string
    switch t361.(type) {
    case Result__unit__string_Ok:
        t362 = "ok"
    case Result__unit__string_Err:
        var inline733 string = t361.(Result__unit__string_Err)._0
        var inline735 string = "err " + inline733
        t362 = inline735
    default:
        panic("non-exhaustive match")
    }
    var inline729 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t362)
    _goml_runtime_std_io_println(inline729)
    var t363 Result__unit__string
    var inline724 string = "goml-self-host/nested/output.txt"
    var inline725 string = "boot"
    var inline726 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline725)
    var inline727 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline724, inline726)
    t363 = inline727
    var t364 string
    switch t363.(type) {
    case Result__unit__string_Ok:
        t364 = "ok"
    case Result__unit__string_Err:
        var inline720 string = t363.(Result__unit__string_Err)._0
        var inline722 string = "err " + inline720
        t364 = inline722
    default:
        panic("non-exhaustive match")
    }
    var inline716 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t364)
    _goml_runtime_std_io_println(inline716)
    var t365 Result__string__string
    var inline707 string = "goml-self-host/nested/output.txt"
    var inline708 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline707)
    switch inline708.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline709 _goml_m_std_p_bytes_p_Bytes = inline708.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline711 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline709)
        t365 = inline711
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline712 string = inline708.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline714 Result__string__string = Result__string__string_Err{
            _0: inline712,
        }
        t365 = inline714
    default:
        panic("non-exhaustive match")
    }
    var t366 string
    switch t365.(type) {
    case Result__string__string_Ok:
        var inline701 string = t365.(Result__string__string_Ok)._0
        t366 = inline701
    case Result__string__string_Err:
        var inline703 string = t365.(Result__string__string_Err)._0
        var inline705 string = "err " + inline703
        t366 = inline705
    default:
        panic("non-exhaustive match")
    }
    var inline698 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t366)
    _goml_runtime_std_io_println(inline698)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop410:
    for {
        var t411 int
        var inline755 int = _goml_runtime_core_string_len(x12)
        t411 = inline755
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

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t418 string
    t418 = value__1
    _goml_runtime_std_io_println(t418)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t428 bool = string_is_char_boundary(value__21, start__22)
    var jp425 bool
    if t428 {
        var t429 bool = string_is_char_boundary(value__21, end__23)
        jp425 = t429
    } else {
        jp425 = false
    }
    if jp425 {
        var t426 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t426
    } else {
        var t427 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t427
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t431 string
    t431 = value__2
    _goml_runtime_std_io_eprint(t431)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t442 int = _goml_runtime_core_string_len(self__38)
    return t442
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t561 bool = index__6 < 0
    var jp559 bool
    if t561 {
        jp559 = true
    } else {
        var t562 bool = index__6 >= length__7
        jp559 = t562
    }
    if jp559 {
        var inline760 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline760
    } else {
        var t446 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t446))
        var t449 bool = first__8 < 128
        if t449 {
            var inline762 int = 1
            var inline763 Option__char = char_from_uint32(first__8)
            switch inline763.(type) {
            case Option__char_None:
                var inline764 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline764
            case Option__char_Some:
                var inline765 rune = inline763.(Option__char_Some)._0
                var inline767 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline765,
                    _2: inline762,
                }
                return inline767
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t453 bool = first__8 < 194
            if t453 {
                var inline769 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline769
            } else {
                var t457 bool = first__8 < 224
                if t457 {
                    var t470 int = length__7 - index__6
                    var t471 bool = t470 < 2
                    if t471 {
                        var inline771 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline771
                    } else {
                        var t459 int = index__6 + 1
                        var t460 uint8
                        var inline785 uint8 = _goml_runtime_core_string_byte_get(value__5, t459)
                        t460 = inline785
                        var second__9 uint32 = uint32(uint8(t460))
                        var t463 bool
                        var inline782 bool = second__9 < 128
                        if inline782 {
                            t463 = true
                        } else {
                            var inline783 bool = second__9 > 191
                            t463 = inline783
                        }
                        if t463 {
                            var inline773 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline773
                        } else {
                            var t465_rhs uint32 = 31
                            var t465 uint32 = first__8 & t465_rhs
                            var t466_rhs int = 6
                            var t466 uint32 = t465 << t466_rhs
                            var t467_rhs uint32 = 63
                            var t467 uint32 = second__9 & t467_rhs
                            var t468 uint32 = t466 | t467
                            var inline775 int = 2
                            var inline776 Option__char = char_from_uint32(t468)
                            switch inline776.(type) {
                            case Option__char_None:
                                var inline777 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline777
                            case Option__char_Some:
                                var inline778 rune = inline776.(Option__char_Some)._0
                                var inline780 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline778,
                                    _2: inline775,
                                }
                                return inline780
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t475 bool = first__8 < 240
                    if t475 {
                        var t508 int = length__7 - index__6
                        var t509 bool = t508 < 3
                        if t509 {
                            var inline787 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline787
                        } else {
                            var t477 int = index__6 + 1
                            var t478 uint8
                            var inline802 uint8 = _goml_runtime_core_string_byte_get(value__5, t477)
                            t478 = inline802
                            var second__10 uint32 = uint32(uint8(t478))
                            var t479 int = index__6 + 2
                            var t480 uint8
                            var inline800 uint8 = _goml_runtime_core_string_byte_get(value__5, t479)
                            t480 = inline800
                            var third__11 uint32 = uint32(uint8(t480))
                            var t506 bool = utf8_invalid_continuation(second__10)
                            var jp501 bool
                            if t506 {
                                jp501 = true
                            } else {
                                var inline789 bool = third__11 < 128
                                if inline789 {
                                    jp501 = true
                                } else {
                                    var inline790 bool = third__11 > 191
                                    jp501 = inline790
                                }
                            }
                            var jp495 bool
                            if jp501 {
                                jp495 = true
                            } else {
                                var t504 bool
                                var inline792 uint32 = 224
                                var inline793 bool = first__8 == inline792
                                t504 = inline793
                                if t504 {
                                    var t505 bool = second__10 < 160
                                    jp495 = t505
                                } else {
                                    jp495 = false
                                }
                            }
                            var jp484 bool
                            if jp495 {
                                jp484 = true
                            } else {
                                var t498 bool
                                var inline795 uint32 = 237
                                var inline796 bool = first__8 == inline795
                                t498 = inline796
                                if t498 {
                                    var t499 bool = second__10 >= 160
                                    jp484 = t499
                                } else {
                                    jp484 = false
                                }
                            }
                            if jp484 {
                                var inline798 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline798
                            } else {
                                var t486_rhs uint32 = 15
                                var t486 uint32 = first__8 & t486_rhs
                                var t487_rhs int = 12
                                var t487 uint32 = t486 << t487_rhs
                                var t488_rhs uint32 = 63
                                var t488 uint32 = second__10 & t488_rhs
                                var t489_rhs int = 6
                                var t489 uint32 = t488 << t489_rhs
                                var t490 uint32 = t487 | t489
                                var t491_rhs uint32 = 63
                                var t491 uint32 = third__11 & t491_rhs
                                var t492 uint32 = t490 | t491
                                var t493 Tuple3_4bool_4char_3int = utf8_valid_decode(t492, 3)
                                return t493
                            }
                        }
                    } else {
                        var t513 bool = first__8 < 245
                        if t513 {
                            var t554 int = length__7 - index__6
                            var t555 bool = t554 < 4
                            if t555 {
                                var t556 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t556
                            } else {
                                var t515 int = index__6 + 1
                                var t516 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t515)
                                var second__12 uint32 = uint32(uint8(t516))
                                var t517 int = index__6 + 2
                                var t518 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t517)
                                var third__13 uint32 = uint32(uint8(t518))
                                var t519 int = index__6 + 3
                                var t520 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t519)
                                var fourth__14 uint32 = uint32(uint8(t520))
                                var t552 bool = utf8_invalid_continuation(second__12)
                                var jp550 bool
                                if t552 {
                                    jp550 = true
                                } else {
                                    var t553 bool = utf8_invalid_continuation(third__13)
                                    jp550 = t553
                                }
                                var jp544 bool
                                if jp550 {
                                    jp544 = true
                                } else {
                                    var t551 bool = utf8_invalid_continuation(fourth__14)
                                    jp544 = t551
                                }
                                var jp538 bool
                                if jp544 {
                                    jp538 = true
                                } else {
                                    var t547 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t547 {
                                        var t548 bool = second__12 < 144
                                        jp538 = t548
                                    } else {
                                        jp538 = false
                                    }
                                }
                                var jp524 bool
                                if jp538 {
                                    jp524 = true
                                } else {
                                    var t541 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t541 {
                                        var t542 bool = second__12 > 143
                                        jp524 = t542
                                    } else {
                                        jp524 = false
                                    }
                                }
                                if jp524 {
                                    var t525 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t525
                                } else {
                                    var t526_rhs uint32 = 7
                                    var t526 uint32 = first__8 & t526_rhs
                                    var t527_rhs int = 18
                                    var t527 uint32 = t526 << t527_rhs
                                    var t528_rhs uint32 = 63
                                    var t528 uint32 = second__12 & t528_rhs
                                    var t529_rhs int = 12
                                    var t529 uint32 = t528 << t529_rhs
                                    var t530 uint32 = t527 | t529
                                    var t531_rhs uint32 = 63
                                    var t531 uint32 = third__13 & t531_rhs
                                    var t532_rhs int = 6
                                    var t532 uint32 = t531 << t532_rhs
                                    var t533 uint32 = t530 | t532
                                    var t534_rhs uint32 = 63
                                    var t534 uint32 = fourth__14 & t534_rhs
                                    var t535 uint32 = t533 | t534
                                    var t536 Tuple3_4bool_4char_3int = utf8_valid_decode(t535, 4)
                                    return t536
                                }
                            }
                        } else {
                            var t557 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t557
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
    var t579 bool = index__16 < 0
    var jp570 bool
    if t579 {
        jp570 = true
    } else {
        var t580 int
        var inline804 int = _goml_runtime_core_string_len(value__15)
        t580 = inline804
        var t581 bool = index__16 > t580
        jp570 = t581
    }
    if jp570 {
        return false
    } else {
        var t573 int
        var inline813 int = _goml_runtime_core_string_len(value__15)
        t573 = inline813
        var t574 bool
        var inline811 bool = index__16 == t573
        t574 = inline811
        if t574 {
            return true
        } else {
            var t575 uint8
            var inline809 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t575 = inline809
            var t576_rhs uint8 = 192
            var t576 uint8 = t575 & t576_rhs
            var t577 bool
            var inline806 uint8 = 128
            var inline807 bool = t576 == inline806
            t577 = inline807
            var t578 bool = !t577
            return t578
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t584 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t584
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t587 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t587
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field835 rune
    var inline817 bool = utf8_valid_scalar(value__0)
    if inline817 {
        var inline818 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline820 rune = inline818._1
        commute_field835 = inline820
        var t593 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field835,
            _2: width__1,
        }
        return t593
    } else {
        var inline815 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline815
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t598 bool = value__3 < 128
    if t598 {
        return true
    } else {
        var t599 bool = value__3 > 191
        return t599
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t602 bool = self__102 == other__103
    return t602
}

func char_from_uint32(value__32 uint32) Option__char {
    var t610 bool
    var inline824 bool = value__32 <= 1114111
    if inline824 {
        var inline825 bool = value__32 >= 55296
        var inline827 bool
        if inline825 {
            var inline829 bool = value__32 <= 57343
            inline827 = inline829
        } else {
            inline827 = false
        }
        var inline828 bool = !inline827
        t610 = inline828
    } else {
        t610 = false
    }
    if t610 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t611 Option__char = Option__char_Some{
            _0: x24,
        }
        return t611
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t616 bool = value__4 <= 1114111
    if t616 {
        var t620 bool = value__4 >= 55296
        var jp618 bool
        if t620 {
            var t621 bool = value__4 <= 57343
            jp618 = t621
        } else {
            jp618 = false
        }
        var t619 bool = !jp618
        return t619
    } else {
        return false
    }
}

func main() {
    main0()
}
