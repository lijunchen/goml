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

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_with_capacity__Vec_5int32(capacity int) *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: make([]int32, 0, capacity),
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
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
    var t808 string
    var inline949 int32 = 7
    var inline950 string = __goml_builtin_int32_to_string(inline949)
    t808 = inline950
    var inline946 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline946)
    var get__1 func(*_goml_vec_int32, int) int32 = vec_get__Vec_5int32
    var push__2 func(*_goml_vec_int32, int32) struct{} = vec_push__Vec_5int32
    var values__3 *_goml_vec_int32
    var inline944 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline944
    push__2(values__3, 11)
    push__2(values__3, 22)
    var t809 int32 = get__1(values__3, 1)
    var inline941 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t809)
    _goml_runtime_core_string_println(inline941)
    var make_slice__4 func(*_goml_vec_int32, int, int) []int32 = func(p0 *_goml_vec_int32, p1 int, p2 int) []int32 {
        return p0.items[p1:p2]
    }
    var slice_get_value__5 func([]int32, int) int32 = func(p0 []int32, p1 int) int32 {
        return p0[p1]
    }
    var view__6 []int32 = make_slice__4(values__3, 0, 2)
    var t810 int32 = slice_get_value__5(view__6, 0)
    var inline938 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t810)
    _goml_runtime_core_string_println(inline938)
    var inline935 int32 = 33
    vec_push__Vec_5int32(values__3, inline935)
    var t811 int
    var inline933 int = vec_len__Vec_5int32(values__3)
    t811 = inline933
    var inline930 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t811)
    _goml_runtime_core_string_println(inline930)
    var inline926 int = 0
    var inline927 int32 = 44
    vec_set__Vec_5int32(values__3, inline926, inline927)
    var t812 int32
    var inline923 int = 0
    var inline924 int32 = vec_get__Vec_5int32(values__3, inline923)
    t812 = inline924
    var inline920 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t812)
    _goml_runtime_core_string_println(inline920)
    var copied__8 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(values__3, 55)
    var t813 int
    var inline918 int = vec_len__Vec_5int32(values__3)
    t813 = inline918
    var inline915 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t813)
    _goml_runtime_core_string_println(inline915)
    var t814 int
    var inline913 int = vec_len__Vec_5int32(copied__8)
    t814 = inline913
    var inline910 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t814)
    _goml_runtime_core_string_println(inline910)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(self__513 *_goml_vec_int32, elem__514 int32) *_goml_vec_int32 {
    var t842 int
    var inline968 int = vec_len__Vec_5int32(self__513)
    t842 = inline968
    var t843 int = t842 + 1
    var result__515 *_goml_vec_int32
    var inline966 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t843)
    result__515 = inline966
    var index__516 int = 0
    Loop_loop845:
    for {
        var t846 int
        var inline962 int = vec_len__Vec_5int32(self__513)
        t846 = inline962
        var t847 bool = index__516 < t846
        if t847 {
            var t848 int32 = vec_get__Vec_5int32(self__513, index__516)
            vec_push__Vec_5int32(result__515, t848)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t849 int = compound_old575 + compound_value576
            index__516 = t849
            continue
        } else {
            break Loop_loop845
        }
    }
    vec_push__Vec_5int32(result__515, elem__514)
    return result__515
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t853 int64 = int64(int32(value__225))
    var inline970 bool = t853 < 0
    if inline970 {
        var inline971 uint64 = uint64(int64(t853))
        var inline972 uint64 = 0 - inline971
        var inline973 string = decimal_string(inline972)
        var inline974 string = "-" + inline973
        return inline974
    } else {
        var inline975 uint64 = uint64(int64(t853))
        var inline976 string = decimal_string(inline975)
        return inline976
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline978 int64 = int64(int32(self__407))
    var inline979 string = signed_decimal_string(inline978)
    return inline979
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline981 int64 = int64(int(self__404))
    var inline982 string = signed_decimal_string(inline981)
    return inline982
}

func signed_decimal_string(value__214 int64) string {
    var t870 bool = value__214 < 0
    if t870 {
        var t871 uint64 = uint64(int64(value__214))
        var t872 uint64 = 0 - t871
        var t873 string = decimal_string(t872)
        var t874 string = "-" + t873
        return t874
    } else {
        var t875 uint64 = uint64(int64(value__214))
        var t876 string = decimal_string(t875)
        return t876
    }
}

func decimal_string(value__208 uint64) string {
    var t903 bool = value__208 == 0
    if t903 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop896:
        for {
            var t897 bool = remaining__210 > 0
            if t897 {
                var t898_rhs uint64 = 10
                var t898 uint64 = remaining__210 % t898_rhs
                var t899 uint8 = uint8(uint64(t898))
                var t900 uint8 = t899 + 48
                vec_push__Vec_5uint8(reversed__209, t900)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t901 uint64 = compound_old353 / compound_value354
                remaining__210 = t901
                continue
            } else {
                break Loop_loop896
            }
        }
        var t885 int
        var inline1000 int = vec_len__Vec_5uint8(reversed__209)
        t885 = inline1000
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t885)
        var offset__212 int = 0
        Loop_loop887:
        for {
            var t888 int
            var inline998 int = vec_len__Vec_5uint8(reversed__209)
            t888 = inline998
            var t889 bool = offset__212 < t888
            if t889 {
                var t890 int
                var inline996 int = vec_len__Vec_5uint8(reversed__209)
                t890 = inline996
                var t891 int = t890 - offset__212
                var t892 int = t891 - 1
                var t893 uint8 = vec_get__Vec_5uint8(reversed__209, t892)
                vec_push__Vec_5uint8(bytes__211, t893)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t894 int = compound_old358 + compound_value359
                offset__212 = t894
                continue
            } else {
                break Loop_loop887
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
