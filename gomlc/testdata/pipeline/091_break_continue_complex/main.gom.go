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

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_os.Stdout.WriteString(s)
    return struct{}{}
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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
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
    var sum__0 *ref_int_x
    var inline954 int = 0
    var inline955 *ref_int_x = ref__Ref_3int(inline954)
    sum__0 = inline955
    var i__1 *ref_int_x
    var inline951 int = 1
    var inline952 *ref_int_x = ref__Ref_3int(inline951)
    i__1 = inline952
    Loop_loop823:
    for {
        var t824 int
        var inline915 int = ref_get__Ref_3int(i__1)
        t824 = inline915
        var t825 bool = t824 <= 100
        if t825 {
            var t832 int
            var inline913 int = ref_get__Ref_3int(i__1)
            t832 = inline913
            var t833 bool = t832 == 50
            if t833 {
                break Loop_loop823
            } else {
                var t827 int
                var inline911 int = ref_get__Ref_3int(sum__0)
                t827 = inline911
                var t828 int
                var inline909 int = ref_get__Ref_3int(i__1)
                t828 = inline909
                var t829 int = t827 + t828
                ref_set__Ref_3int(sum__0, t829)
                var t830 int
                var inline905 int = ref_get__Ref_3int(i__1)
                t830 = inline905
                var t831 int = t830 + 1
                ref_set__Ref_3int(i__1, t831)
                continue
            }
        } else {
            break Loop_loop823
        }
    }
    var inline947 string = "sum up to break: "
    var inline948 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline947)
    _goml_runtime_core_string_print(inline948)
    var t810 int
    var inline945 int = ref_get__Ref_3int(sum__0)
    t810 = inline945
    var inline942 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t810)
    _goml_runtime_core_string_println(inline942)
    var even_sum__2 *ref_int_x
    var inline939 int = 0
    var inline940 *ref_int_x = ref__Ref_3int(inline939)
    even_sum__2 = inline940
    var j__3 *ref_int_x
    var inline936 int = 1
    var inline937 *ref_int_x = ref__Ref_3int(inline936)
    j__3 = inline937
    Loop_loop813:
    for {
        var t814 int
        var inline925 int = ref_get__Ref_3int(j__3)
        t814 = inline925
        var t815 bool = t814 <= 10
        if t815 {
            var cur__4 int
            var inline923 int = ref_get__Ref_3int(j__3)
            cur__4 = inline923
            var t816 int = cur__4 + 1
            ref_set__Ref_3int(j__3, t816)
            var t818 int = cur__4 / 2
            var t819 int = t818 * 2
            var t820 bool = cur__4 == t819
            if t820 {
                var t821 int
                var inline919 int = ref_get__Ref_3int(even_sum__2)
                t821 = inline919
                var t822 int = t821 + cur__4
                ref_set__Ref_3int(even_sum__2, t822)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop813
        }
    }
    var inline932 string = "even sum: "
    var inline933 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline932)
    _goml_runtime_core_string_print(inline933)
    var t812 int
    var inline930 int = ref_get__Ref_3int(even_sum__2)
    t812 = inline930
    var inline927 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t812)
    _goml_runtime_core_string_println(inline927)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline960 int64 = int64(int(self__404))
    var inline961 string = signed_decimal_string(inline960)
    return inline961
}

func signed_decimal_string(value__214 int64) string {
    var t861 bool = value__214 < 0
    if t861 {
        var t862 uint64 = uint64(int64(value__214))
        var t863 uint64 = 0 - t862
        var t864 string = decimal_string(t863)
        var t865 string = "-" + t864
        return t865
    } else {
        var t866 uint64 = uint64(int64(value__214))
        var t867 string = decimal_string(t866)
        return t867
    }
}

func decimal_string(value__208 uint64) string {
    var t890 bool = value__208 == 0
    if t890 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop883:
        for {
            var t884 bool = remaining__210 > 0
            if t884 {
                var t885_rhs uint64 = 10
                var t885 uint64 = remaining__210 % t885_rhs
                var t886 uint8 = uint8(uint64(t885))
                var t887 uint8 = t886 + 48
                vec_push__Vec_5uint8(reversed__209, t887)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t888 uint64 = compound_old353 / compound_value354
                remaining__210 = t888
                continue
            } else {
                break Loop_loop883
            }
        }
        var t872 int
        var inline979 int = vec_len__Vec_5uint8(reversed__209)
        t872 = inline979
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t872)
        var offset__212 int = 0
        Loop_loop874:
        for {
            var t875 int
            var inline977 int = vec_len__Vec_5uint8(reversed__209)
            t875 = inline977
            var t876 bool = offset__212 < t875
            if t876 {
                var t877 int
                var inline975 int = vec_len__Vec_5uint8(reversed__209)
                t877 = inline975
                var t878 int = t877 - offset__212
                var t879 int = t878 - 1
                var t880 uint8 = vec_get__Vec_5uint8(reversed__209, t879)
                vec_push__Vec_5uint8(bytes__211, t880)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t881 int = compound_old358 + compound_value359
                offset__212 = t881
                continue
            } else {
                break Loop_loop874
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
