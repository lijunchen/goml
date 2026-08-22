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

type Ordering int32

const (
    WRAPPED uint8 = 255
    LETTER uint32 = 65
)

func main0() struct{} {
    var value__11 int16 = 511
    var t914 uint8 = uint8(int16(value__11))
    var inline1003 string = _goml_m_trait__impl_i_ToString_i_u8_i_to__string(t914)
    _goml_runtime_core_string_println(inline1003)
    var inline1000 string = _goml_m_trait__impl_i_ToString_i_u8_i_to__string(WRAPPED)
    _goml_runtime_core_string_println(inline1000)
    var inline997 string = _goml_m_trait__impl_i_ToString_i_u32_i_to__string(LETTER)
    _goml_runtime_core_string_println(inline997)
    var t915 uint8
    var inline994 int16 = -1
    var inline995 uint8 = uint8(int16(inline994))
    t915 = inline995
    var inline991 string = _goml_m_trait__impl_i_ToString_i_u8_i_to__string(t915)
    _goml_runtime_core_string_println(inline991)
    var octet__12 uint8 = 255
    var t916 int16 = int16(uint8(octet__12))
    var inline988 string = _goml_m_trait__impl_i_ToString_i_i16_i_to__string(t916)
    _goml_runtime_core_string_println(inline988)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_u8_i_to__string(self__409 uint8) string {
    var inline1032 uint64 = uint64(uint8(self__409))
    var inline1033 string = decimal_string(inline1032)
    return inline1033
}

func _goml_m_trait__impl_i_ToString_i_u32_i_to__string(self__411 uint32) string {
    var inline1035 uint64 = uint64(uint32(self__411))
    var inline1036 string = decimal_string(inline1035)
    return inline1036
}

func _goml_m_trait__impl_i_ToString_i_i16_i_to__string(self__406 int16) string {
    var inline1038 int64 = int64(int16(self__406))
    var inline1039 string = signed_decimal_string(inline1038)
    return inline1039
}

func decimal_string(value__208 uint64) string {
    var t970 bool = value__208 == 0
    if t970 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop963:
        for {
            var t964 bool = remaining__210 > 0
            if t964 {
                var t965_rhs uint64 = 10
                var t965 uint64 = remaining__210 % t965_rhs
                var t966 uint8 = uint8(uint64(t965))
                var t967 uint8 = t966 + 48
                vec_push__Vec_5uint8(reversed__209, t967)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t968 uint64 = compound_old353 / compound_value354
                remaining__210 = t968
                continue
            } else {
                break Loop_loop963
            }
        }
        var t952 int
        var inline1057 int = vec_len__Vec_5uint8(reversed__209)
        t952 = inline1057
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t952)
        var offset__212 int = 0
        Loop_loop954:
        for {
            var t955 int
            var inline1055 int = vec_len__Vec_5uint8(reversed__209)
            t955 = inline1055
            var t956 bool = offset__212 < t955
            if t956 {
                var t957 int
                var inline1053 int = vec_len__Vec_5uint8(reversed__209)
                t957 = inline1053
                var t958 int = t957 - offset__212
                var t959 int = t958 - 1
                var t960 uint8 = vec_get__Vec_5uint8(reversed__209, t959)
                vec_push__Vec_5uint8(bytes__211, t960)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t961 int = compound_old358 + compound_value359
                offset__212 = t961
                continue
            } else {
                break Loop_loop954
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func signed_decimal_string(value__214 int64) string {
    var t975 bool = value__214 < 0
    if t975 {
        var t976 uint64 = uint64(int64(value__214))
        var t977 uint64 = 0 - t976
        var t978 string = decimal_string(t977)
        var t979 string = "-" + t978
        return t979
    } else {
        var t980 uint64 = uint64(int64(value__214))
        var t981 string = decimal_string(t980)
        return t981
    }
}

func main() {
    main0()
}
