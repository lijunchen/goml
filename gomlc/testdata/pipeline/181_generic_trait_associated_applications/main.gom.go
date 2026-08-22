package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type Value struct {}

type Ordering int32

func main0() struct{} {
    var text__2 string
    text__2 = "isize"
    var number__3 int32
    number__3 = 7
    var inline863 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__2)
    _goml_runtime_core_string_println(inline863)
    var inline860 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(number__3)
    _goml_runtime_core_string_println(inline860)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline871 int64 = int64(int32(self__407))
    var inline872 string = signed_decimal_string(inline871)
    return inline872
}

func signed_decimal_string(value__214 int64) string {
    var t824 bool = value__214 < 0
    if t824 {
        var t825 uint64 = uint64(int64(value__214))
        var t826 uint64 = 0 - t825
        var t827 string = decimal_string(t826)
        var t828 string = "-" + t827
        return t828
    } else {
        var t829 uint64 = uint64(int64(value__214))
        var t830 string = decimal_string(t829)
        return t830
    }
}

func decimal_string(value__208 uint64) string {
    var t853 bool = value__208 == 0
    if t853 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop846:
        for {
            var t847 bool = remaining__210 > 0
            if t847 {
                var t848_rhs uint64 = 10
                var t848 uint64 = remaining__210 % t848_rhs
                var t849 uint8 = uint8(uint64(t848))
                var t850 uint8 = t849 + 48
                vec_push__Vec_5uint8(reversed__209, t850)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t851 uint64 = compound_old353 / compound_value354
                remaining__210 = t851
                continue
            } else {
                break Loop_loop846
            }
        }
        var t835 int
        var inline890 int = vec_len__Vec_5uint8(reversed__209)
        t835 = inline890
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t835)
        var offset__212 int = 0
        Loop_loop837:
        for {
            var t838 int
            var inline888 int = vec_len__Vec_5uint8(reversed__209)
            t838 = inline888
            var t839 bool = offset__212 < t838
            if t839 {
                var t840 int
                var inline886 int = vec_len__Vec_5uint8(reversed__209)
                t840 = inline886
                var t841 int = t840 - offset__212
                var t842 int = t841 - 1
                var t843 uint8 = vec_get__Vec_5uint8(reversed__209, t842)
                vec_push__Vec_5uint8(bytes__211, t843)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t844 int = compound_old358 + compound_value359
                offset__212 = t844
                continue
            } else {
                break Loop_loop837
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
