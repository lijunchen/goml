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
    var t193 *_goml_vec_uint8
    var inline622 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t193 = inline622
    var t194 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t193,
    }
    return t194
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t238 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t238)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t241 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t241
    } else {
        var t242 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t242
    }
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    if x4 {
        var t257 _goml_m_std_p_bytes_p_Bytes
        var inline681 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x5,
        }
        t257 = inline681
        var t258 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t257,
        }
        return t258
    } else {
        var t259 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x6,
        }
        return t259
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t262 *_goml_vec_uint8
    var inline683 *_goml_vec_uint8 = data__10.values
    t262 = inline683
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t262)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    if x8 {
        var t265 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t265
    } else {
        var t266 Result__unit__string = Result__unit__string_Err{
            _0: x9,
        }
        return t266
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__13 string) Result__unit__string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__13)
    var x11 bool = mtmp10._0
    var x12 string = mtmp10._1
    if x11 {
        var t271 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t271
    } else {
        var t272 Result__unit__string = Result__unit__string_Err{
            _0: x12,
        }
        return t272
    }
}

func main0() struct{} {
    var t355 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t355)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline732 string = ""
    var inline733 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline732)
    var inline734 string = inline733 + "\n"
    _goml_runtime_std_io_eprint(inline734)
    var t356 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t357 string
    switch t356.(type) {
    case Result__unit__string_Ok:
        t357 = "ok"
    case Result__unit__string_Err:
        var inline728 string = t356.(Result__unit__string_Err)._0
        var inline730 string = "err " + inline728
        t357 = inline730
    default:
        panic("non-exhaustive match")
    }
    var inline724 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t357)
    _goml_runtime_std_io_println(inline724)
    var t358 Result__unit__string
    var inline719 string = "goml-self-host/nested/output.txt"
    var inline720 string = "boot"
    var inline721 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline720)
    var inline722 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline719, inline721)
    t358 = inline722
    var t359 string
    switch t358.(type) {
    case Result__unit__string_Ok:
        t359 = "ok"
    case Result__unit__string_Err:
        var inline715 string = t358.(Result__unit__string_Err)._0
        var inline717 string = "err " + inline715
        t359 = inline717
    default:
        panic("non-exhaustive match")
    }
    var inline711 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t359)
    _goml_runtime_std_io_println(inline711)
    var t360 Result__string__string
    var inline702 string = "goml-self-host/nested/output.txt"
    var inline703 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline702)
    switch inline703.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline704 _goml_m_std_p_bytes_p_Bytes = inline703.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline706 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline704)
        t360 = inline706
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline707 string = inline703.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline709 Result__string__string = Result__string__string_Err{
            _0: inline707,
        }
        t360 = inline709
    default:
        panic("non-exhaustive match")
    }
    var t361 string
    switch t360.(type) {
    case Result__string__string_Ok:
        var inline696 string = t360.(Result__string__string_Ok)._0
        t361 = inline696
    case Result__string__string_Err:
        var inline698 string = t360.(Result__string__string_Err)._0
        var inline700 string = "err " + inline698
        t361 = inline700
    default:
        panic("non-exhaustive match")
    }
    var inline693 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t361)
    _goml_runtime_std_io_println(inline693)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop405:
    for {
        var t406 int
        var inline750 int = _goml_runtime_core_string_len(x12)
        t406 = inline750
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

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t413 string
    t413 = value__1
    _goml_runtime_std_io_println(t413)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t423 bool = string_is_char_boundary(value__21, start__22)
    var jp420 bool
    if t423 {
        var t424 bool = string_is_char_boundary(value__21, end__23)
        jp420 = t424
    } else {
        jp420 = false
    }
    if jp420 {
        var t421 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t421
    } else {
        var t422 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t422
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__2 string) struct{} {
    var t426 string
    t426 = value__2
    _goml_runtime_std_io_eprint(t426)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t437 int = _goml_runtime_core_string_len(self__38)
    return t437
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t556 bool = index__6 < 0
    var jp554 bool
    if t556 {
        jp554 = true
    } else {
        var t557 bool = index__6 >= length__7
        jp554 = t557
    }
    if jp554 {
        var inline755 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline755
    } else {
        var t441 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t441))
        var t444 bool = first__8 < 128
        if t444 {
            var inline757 int = 1
            var inline758 Option__char = char_from_uint32(first__8)
            switch inline758.(type) {
            case Option__char_None:
                var inline759 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline759
            case Option__char_Some:
                var inline760 rune = inline758.(Option__char_Some)._0
                var inline762 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline760,
                    _2: inline757,
                }
                return inline762
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t448 bool = first__8 < 194
            if t448 {
                var inline764 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline764
            } else {
                var t452 bool = first__8 < 224
                if t452 {
                    var t465 int = length__7 - index__6
                    var t466 bool = t465 < 2
                    if t466 {
                        var inline766 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline766
                    } else {
                        var t454 int = index__6 + 1
                        var t455 uint8
                        var inline780 uint8 = _goml_runtime_core_string_byte_get(value__5, t454)
                        t455 = inline780
                        var second__9 uint32 = uint32(uint8(t455))
                        var t458 bool
                        var inline777 bool = second__9 < 128
                        if inline777 {
                            t458 = true
                        } else {
                            var inline778 bool = second__9 > 191
                            t458 = inline778
                        }
                        if t458 {
                            var inline768 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline768
                        } else {
                            var t460_rhs uint32 = 31
                            var t460 uint32 = first__8 & t460_rhs
                            var t461_rhs int = 6
                            var t461 uint32 = t460 << t461_rhs
                            var t462_rhs uint32 = 63
                            var t462 uint32 = second__9 & t462_rhs
                            var t463 uint32 = t461 | t462
                            var inline770 int = 2
                            var inline771 Option__char = char_from_uint32(t463)
                            switch inline771.(type) {
                            case Option__char_None:
                                var inline772 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline772
                            case Option__char_Some:
                                var inline773 rune = inline771.(Option__char_Some)._0
                                var inline775 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline773,
                                    _2: inline770,
                                }
                                return inline775
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t470 bool = first__8 < 240
                    if t470 {
                        var t503 int = length__7 - index__6
                        var t504 bool = t503 < 3
                        if t504 {
                            var inline782 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline782
                        } else {
                            var t472 int = index__6 + 1
                            var t473 uint8
                            var inline797 uint8 = _goml_runtime_core_string_byte_get(value__5, t472)
                            t473 = inline797
                            var second__10 uint32 = uint32(uint8(t473))
                            var t474 int = index__6 + 2
                            var t475 uint8
                            var inline795 uint8 = _goml_runtime_core_string_byte_get(value__5, t474)
                            t475 = inline795
                            var third__11 uint32 = uint32(uint8(t475))
                            var t501 bool = utf8_invalid_continuation(second__10)
                            var jp496 bool
                            if t501 {
                                jp496 = true
                            } else {
                                var inline784 bool = third__11 < 128
                                if inline784 {
                                    jp496 = true
                                } else {
                                    var inline785 bool = third__11 > 191
                                    jp496 = inline785
                                }
                            }
                            var jp490 bool
                            if jp496 {
                                jp490 = true
                            } else {
                                var t499 bool
                                var inline787 uint32 = 224
                                var inline788 bool = first__8 == inline787
                                t499 = inline788
                                if t499 {
                                    var t500 bool = second__10 < 160
                                    jp490 = t500
                                } else {
                                    jp490 = false
                                }
                            }
                            var jp479 bool
                            if jp490 {
                                jp479 = true
                            } else {
                                var t493 bool
                                var inline790 uint32 = 237
                                var inline791 bool = first__8 == inline790
                                t493 = inline791
                                if t493 {
                                    var t494 bool = second__10 >= 160
                                    jp479 = t494
                                } else {
                                    jp479 = false
                                }
                            }
                            if jp479 {
                                var inline793 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline793
                            } else {
                                var t481_rhs uint32 = 15
                                var t481 uint32 = first__8 & t481_rhs
                                var t482_rhs int = 12
                                var t482 uint32 = t481 << t482_rhs
                                var t483_rhs uint32 = 63
                                var t483 uint32 = second__10 & t483_rhs
                                var t484_rhs int = 6
                                var t484 uint32 = t483 << t484_rhs
                                var t485 uint32 = t482 | t484
                                var t486_rhs uint32 = 63
                                var t486 uint32 = third__11 & t486_rhs
                                var t487 uint32 = t485 | t486
                                var t488 Tuple3_4bool_4char_3int = utf8_valid_decode(t487, 3)
                                return t488
                            }
                        }
                    } else {
                        var t508 bool = first__8 < 245
                        if t508 {
                            var t549 int = length__7 - index__6
                            var t550 bool = t549 < 4
                            if t550 {
                                var t551 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t551
                            } else {
                                var t510 int = index__6 + 1
                                var t511 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t510)
                                var second__12 uint32 = uint32(uint8(t511))
                                var t512 int = index__6 + 2
                                var t513 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t512)
                                var third__13 uint32 = uint32(uint8(t513))
                                var t514 int = index__6 + 3
                                var t515 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t514)
                                var fourth__14 uint32 = uint32(uint8(t515))
                                var t547 bool = utf8_invalid_continuation(second__12)
                                var jp545 bool
                                if t547 {
                                    jp545 = true
                                } else {
                                    var t548 bool = utf8_invalid_continuation(third__13)
                                    jp545 = t548
                                }
                                var jp539 bool
                                if jp545 {
                                    jp539 = true
                                } else {
                                    var t546 bool = utf8_invalid_continuation(fourth__14)
                                    jp539 = t546
                                }
                                var jp533 bool
                                if jp539 {
                                    jp533 = true
                                } else {
                                    var t542 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t542 {
                                        var t543 bool = second__12 < 144
                                        jp533 = t543
                                    } else {
                                        jp533 = false
                                    }
                                }
                                var jp519 bool
                                if jp533 {
                                    jp519 = true
                                } else {
                                    var t536 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t536 {
                                        var t537 bool = second__12 > 143
                                        jp519 = t537
                                    } else {
                                        jp519 = false
                                    }
                                }
                                if jp519 {
                                    var t520 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t520
                                } else {
                                    var t521_rhs uint32 = 7
                                    var t521 uint32 = first__8 & t521_rhs
                                    var t522_rhs int = 18
                                    var t522 uint32 = t521 << t522_rhs
                                    var t523_rhs uint32 = 63
                                    var t523 uint32 = second__12 & t523_rhs
                                    var t524_rhs int = 12
                                    var t524 uint32 = t523 << t524_rhs
                                    var t525 uint32 = t522 | t524
                                    var t526_rhs uint32 = 63
                                    var t526 uint32 = third__13 & t526_rhs
                                    var t527_rhs int = 6
                                    var t527 uint32 = t526 << t527_rhs
                                    var t528 uint32 = t525 | t527
                                    var t529_rhs uint32 = 63
                                    var t529 uint32 = fourth__14 & t529_rhs
                                    var t530 uint32 = t528 | t529
                                    var t531 Tuple3_4bool_4char_3int = utf8_valid_decode(t530, 4)
                                    return t531
                                }
                            }
                        } else {
                            var t552 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t552
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
    var t574 bool = index__16 < 0
    var jp565 bool
    if t574 {
        jp565 = true
    } else {
        var t575 int
        var inline799 int = _goml_runtime_core_string_len(value__15)
        t575 = inline799
        var t576 bool = index__16 > t575
        jp565 = t576
    }
    if jp565 {
        return false
    } else {
        var t568 int
        var inline808 int = _goml_runtime_core_string_len(value__15)
        t568 = inline808
        var t569 bool
        var inline806 bool = index__16 == t568
        t569 = inline806
        if t569 {
            return true
        } else {
            var t570 uint8
            var inline804 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t570 = inline804
            var t571_rhs uint8 = 192
            var t571 uint8 = t570 & t571_rhs
            var t572 bool
            var inline801 uint8 = 128
            var inline802 bool = t571 == inline801
            t572 = inline802
            var t573 bool = !t572
            return t573
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t579 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t579
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t582 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t582
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field830 rune
    var inline812 bool = utf8_valid_scalar(value__0)
    if inline812 {
        var inline813 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline815 rune = inline813._1
        commute_field830 = inline815
        var t588 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field830,
            _2: width__1,
        }
        return t588
    } else {
        var inline810 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline810
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t593 bool = value__3 < 128
    if t593 {
        return true
    } else {
        var t594 bool = value__3 > 191
        return t594
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t597 bool = self__117 == other__118
    return t597
}

func char_from_uint32(value__32 uint32) Option__char {
    var t605 bool
    var inline819 bool = value__32 <= 1114111
    if inline819 {
        var inline820 bool = value__32 >= 55296
        var inline822 bool
        if inline820 {
            var inline824 bool = value__32 <= 57343
            inline822 = inline824
        } else {
            inline822 = false
        }
        var inline823 bool = !inline822
        t605 = inline823
    } else {
        t605 = false
    }
    if t605 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t606 Option__char = Option__char_Some{
            _0: x24,
        }
        return t606
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t611 bool = value__4 <= 1114111
    if t611 {
        var t615 bool = value__4 >= 55296
        var jp613 bool
        if t615 {
            var t616 bool = value__4 <= 57343
            jp613 = t616
        } else {
            jp613 = false
        }
        var t614 bool = !jp613
        return t614
    } else {
        return false
    }
}

func main() {
    main0()
}
