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

type _goml_vec_Never struct {
    items []Never
}

type _goml_vec_bool struct {
    items []bool
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

type Never int32

const (

)

type Loop interface {
    isLoop()
}

type Next struct {
    _0 Loop
}

func (_ Next) isLoop() {}

type MaybeNever interface {
    isMaybeNever()
}

type Empty struct {}

func (_ Empty) isMaybeNever() {}

type Filled struct {
    _0 Never
}

func (_ Filled) isMaybeNever() {}

type Single struct {
    _tag int32
    _v0_0 int32
}

func main0() struct{} {
    var t877 int32
    var inline949 int32 = 12
    t877 = inline949
    var inline945 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t877)
    _goml_runtime_core_string_println(inline945)
    var t878 int32
    var inline943 bool = true
    switch inline943 {
    case true:
        t878 = 1
    case false:
        t878 = 2
    default:
        panic("non-exhaustive match")
    }
    var inline940 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t878)
    _goml_runtime_core_string_println(inline940)
    var t879 string
    var inline938 float64 = 0
    switch inline938 {
    case -0:
        t879 = "zero"
    default:
        t879 = "other"
    }
    var inline935 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t879)
    _goml_runtime_core_string_println(inline935)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline955 int64 = int64(int32(self__407))
    var inline956 string = signed_decimal_string(inline955)
    return inline956
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t899 bool = value__214 < 0
    if t899 {
        var t900 uint64 = uint64(int64(value__214))
        var t901 uint64 = 0 - t900
        var t902 string = decimal_string(t901)
        var t903 string = "-" + t902
        return t903
    } else {
        var t904 uint64 = uint64(int64(value__214))
        var t905 string = decimal_string(t904)
        return t905
    }
}

func decimal_string(value__208 uint64) string {
    var t928 bool = value__208 == 0
    if t928 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop921:
        for {
            var t922 bool = remaining__210 > 0
            if t922 {
                var t923_rhs uint64 = 10
                var t923 uint64 = remaining__210 % t923_rhs
                var t924 uint8 = uint8(uint64(t923))
                var t925 uint8 = t924 + 48
                vec_push__Vec_5uint8(reversed__209, t925)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t926 uint64 = compound_old353 / compound_value354
                remaining__210 = t926
                continue
            } else {
                break Loop_loop921
            }
        }
        var t910 int
        var inline974 int = vec_len__Vec_5uint8(reversed__209)
        t910 = inline974
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t910)
        var offset__212 int = 0
        Loop_loop912:
        for {
            var t913 int
            var inline972 int = vec_len__Vec_5uint8(reversed__209)
            t913 = inline972
            var t914 bool = offset__212 < t913
            if t914 {
                var t915 int
                var inline970 int = vec_len__Vec_5uint8(reversed__209)
                t915 = inline970
                var t916 int = t915 - offset__212
                var t917 int = t916 - 1
                var t918 uint8 = vec_get__Vec_5uint8(reversed__209, t917)
                vec_push__Vec_5uint8(bytes__211, t918)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t919 int = compound_old358 + compound_value359
                offset__212 = t919
                continue
            } else {
                break Loop_loop912
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
