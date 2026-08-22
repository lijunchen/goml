package main

import (
    _goml_os "os"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

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

func main0() struct{} {
    var a__7 int32
    var inline904 int32 = 1
    a__7 = inline904
    var b__8 int32
    var inline902 int32 = 2
    b__8 = inline902
    var c__9 int32
    var inline900 int32 = a__7 + b__8
    c__9 = inline900
    var inline897 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(c__9)
    println__T_string(inline897)
    var a__10 int32
    var inline895 int32 = 3
    a__10 = inline895
    var b__11 int32
    var inline893 int32 = 4
    b__11 = inline893
    var c__12 bool
    var inline891 bool = a__10 < b__11
    c__12 = inline891
    var inline888 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline888)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t814 string
    t814 = value__1
    _goml_runtime_core_string_println(t814)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline907 int64 = int64(int32(self__407))
    var inline908 string = signed_decimal_string(inline907)
    return inline908
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t821 string = _goml_runtime_core_bool_to_string(self__401)
    return t821
}

func signed_decimal_string(value__214 int64) string {
    var t838 bool = value__214 < 0
    if t838 {
        var t839 uint64 = uint64(int64(value__214))
        var t840 uint64 = 0 - t839
        var t841 string = decimal_string(t840)
        var t842 string = "-" + t841
        return t842
    } else {
        var t843 uint64 = uint64(int64(value__214))
        var t844 string = decimal_string(t843)
        return t844
    }
}

func decimal_string(value__208 uint64) string {
    var t867 bool = value__208 == 0
    if t867 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop860:
        for {
            var t861 bool = remaining__210 > 0
            if t861 {
                var t862_rhs uint64 = 10
                var t862 uint64 = remaining__210 % t862_rhs
                var t863 uint8 = uint8(uint64(t862))
                var t864 uint8 = t863 + 48
                vec_push__Vec_5uint8(reversed__209, t864)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t865 uint64 = compound_old353 / compound_value354
                remaining__210 = t865
                continue
            } else {
                break Loop_loop860
            }
        }
        var t849 int
        var inline926 int = vec_len__Vec_5uint8(reversed__209)
        t849 = inline926
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t849)
        var offset__212 int = 0
        Loop_loop851:
        for {
            var t852 int
            var inline924 int = vec_len__Vec_5uint8(reversed__209)
            t852 = inline924
            var t853 bool = offset__212 < t852
            if t853 {
                var t854 int
                var inline922 int = vec_len__Vec_5uint8(reversed__209)
                t854 = inline922
                var t855 int = t854 - offset__212
                var t856 int = t855 - 1
                var t857 uint8 = vec_get__Vec_5uint8(reversed__209, t856)
                vec_push__Vec_5uint8(bytes__211, t857)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t858 int = compound_old358 + compound_value359
                offset__212 = t858
                continue
            } else {
                break Loop_loop851
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
