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

func array_get__Array_3_3int(arr [3]int, index int) int {
    return arr[index]
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

type Pair struct {
    left int
    right int
}

type Ordering int32

type Choice struct {
    _tag int32
    _v0_0 int
}

const (
    ANSWER int = 120
)

func factorial(value__0 int) int {
    var t814 bool = value__0 < 2
    if t814 {
        return 1
    } else {
        var t815 int = value__0 - 1
        var t816 int = factorial(t815)
        var t817 int = value__0 * t816
        return t817
    }
}

func main0() struct{} {
    var values__12 [3]int = [3]int{6, 10, 4}
    var inline962 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(ANSWER)
    _goml_runtime_core_string_println(inline962)
    var t849 int = array_get__Array_3_3int(values__12, 0)
    var inline959 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t849)
    _goml_runtime_core_string_println(inline959)
    var t850 int = array_get__Array_3_3int(values__12, 1)
    var inline956 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t850)
    _goml_runtime_core_string_println(inline956)
    var t851 int = array_get__Array_3_3int(values__12, 2)
    var inline953 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t851)
    _goml_runtime_core_string_println(inline953)
    var t852 int = 7
    var t853 int = 8
    var t854 int = t852 + t853
    var inline950 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t854)
    _goml_runtime_core_string_println(inline950)
    var x804 int = 9
    var inline922 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x804)
    _goml_runtime_core_string_println(inline922)
    var t856 int = factorial(5)
    var t857 bool = t856 == ANSWER
    var inline947 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t857)
    _goml_runtime_core_string_println(inline947)
    var shadowed__16 int = 12
    var inline944 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(shadowed__16)
    _goml_runtime_core_string_println(inline944)
    var t858 int
    var inline937 int = 3
    var inline939 int = inline937 + 1
    var inline940 int = inline939 * 2
    var inline942 int = inline939 + inline940
    t858 = inline942
    var t859 bool = t858 == shadowed__16
    var inline934 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t859)
    _goml_runtime_core_string_println(inline934)
    var widened__17 uint64 = 18446744073709551615
    var t860 uint64
    var inline931 int8 = -1
    var inline932 uint64 = uint64(int8(inline931))
    t860 = inline932
    var t861 bool = widened__17 == t860
    var inline928 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t861)
    _goml_runtime_core_string_println(inline928)
    var t862 int = factorial(4)
    var inline925 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t862)
    _goml_runtime_core_string_println(inline925)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline969 int64 = int64(int(self__404))
    var inline970 string = signed_decimal_string(inline969)
    return inline970
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t877 string = _goml_runtime_core_bool_to_string(self__401)
    return t877
}

func signed_decimal_string(value__214 int64) string {
    var t886 bool = value__214 < 0
    if t886 {
        var t887 uint64 = uint64(int64(value__214))
        var t888 uint64 = 0 - t887
        var t889 string = decimal_string(t888)
        var t890 string = "-" + t889
        return t890
    } else {
        var t891 uint64 = uint64(int64(value__214))
        var t892 string = decimal_string(t891)
        return t892
    }
}

func decimal_string(value__208 uint64) string {
    var t915 bool = value__208 == 0
    if t915 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop908:
        for {
            var t909 bool = remaining__210 > 0
            if t909 {
                var t910_rhs uint64 = 10
                var t910 uint64 = remaining__210 % t910_rhs
                var t911 uint8 = uint8(uint64(t910))
                var t912 uint8 = t911 + 48
                vec_push__Vec_5uint8(reversed__209, t912)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t913 uint64 = compound_old353 / compound_value354
                remaining__210 = t913
                continue
            } else {
                break Loop_loop908
            }
        }
        var t897 int
        var inline988 int = vec_len__Vec_5uint8(reversed__209)
        t897 = inline988
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t897)
        var offset__212 int = 0
        Loop_loop899:
        for {
            var t900 int
            var inline986 int = vec_len__Vec_5uint8(reversed__209)
            t900 = inline986
            var t901 bool = offset__212 < t900
            if t901 {
                var t902 int
                var inline984 int = vec_len__Vec_5uint8(reversed__209)
                t902 = inline984
                var t903 int = t902 - offset__212
                var t904 int = t903 - 1
                var t905 uint8 = vec_get__Vec_5uint8(reversed__209, t904)
                vec_push__Vec_5uint8(bytes__211, t905)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t906 int = compound_old358 + compound_value359
                offset__212 = t906
                continue
            } else {
                break Loop_loop899
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
