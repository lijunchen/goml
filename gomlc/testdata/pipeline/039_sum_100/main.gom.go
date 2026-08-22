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

func sum(n__2 int32) int32 {
    var t808 bool
    var inline865 int32 = 1
    var inline866 bool = n__2 < inline865
    var inline867 bool = !inline866
    if inline867 {
        var inline868 bool = inline865 < n__2
        var inline869 bool = !inline868
        t808 = inline869
    } else {
        t808 = false
    }
    if t808 {
        return 1
    } else {
        var t809 int32 = n__2 - 1
        var t810 int32 = sum(t809)
        var t811 int32 = n__2 + t810
        return t811
    }
}

func main0() struct{} {
    var t813 int32 = sum(100)
    var inline871 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t813)
    _goml_runtime_core_string_println(inline871)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline876 int64 = int64(int32(self__407))
    var inline877 string = signed_decimal_string(inline876)
    return inline877
}

func signed_decimal_string(value__214 int64) string {
    var t829 bool = value__214 < 0
    if t829 {
        var t830 uint64 = uint64(int64(value__214))
        var t831 uint64 = 0 - t830
        var t832 string = decimal_string(t831)
        var t833 string = "-" + t832
        return t833
    } else {
        var t834 uint64 = uint64(int64(value__214))
        var t835 string = decimal_string(t834)
        return t835
    }
}

func decimal_string(value__208 uint64) string {
    var t858 bool = value__208 == 0
    if t858 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop851:
        for {
            var t852 bool = remaining__210 > 0
            if t852 {
                var t853_rhs uint64 = 10
                var t853 uint64 = remaining__210 % t853_rhs
                var t854 uint8 = uint8(uint64(t853))
                var t855 uint8 = t854 + 48
                vec_push__Vec_5uint8(reversed__209, t855)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t856 uint64 = compound_old353 / compound_value354
                remaining__210 = t856
                continue
            } else {
                break Loop_loop851
            }
        }
        var t840 int
        var inline895 int = vec_len__Vec_5uint8(reversed__209)
        t840 = inline895
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t840)
        var offset__212 int = 0
        Loop_loop842:
        for {
            var t843 int
            var inline893 int = vec_len__Vec_5uint8(reversed__209)
            t843 = inline893
            var t844 bool = offset__212 < t843
            if t844 {
                var t845 int
                var inline891 int = vec_len__Vec_5uint8(reversed__209)
                t845 = inline891
                var t846 int = t845 - offset__212
                var t847 int = t846 - 1
                var t848 uint8 = vec_get__Vec_5uint8(reversed__209, t847)
                vec_push__Vec_5uint8(bytes__211, t848)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t849 int = compound_old358 + compound_value359
                offset__212 = t849
                continue
            } else {
                break Loop_loop842
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
