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

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
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
    var v__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32()
    var inline949 int32 = 10
    vec_push__Vec_5int32(v__0, inline949)
    var inline946 int32 = 20
    vec_push__Vec_5int32(v__0, inline946)
    var inline943 int32 = 30
    vec_push__Vec_5int32(v__0, inline943)
    var inline940 int32 = 40
    vec_push__Vec_5int32(v__0, inline940)
    var s__1 []int32
    var inline936 int = 1
    var inline937 int = 4
    var inline938 []int32 = v__0.items[inline936:inline937]
    s__1 = inline938
    var t808 int
    var inline934 int = len(s__1)
    t808 = inline934
    var inline931 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t808)
    _goml_runtime_core_string_println(inline931)
    var t809 int32
    var inline928 int = 0
    var inline929 int32 = s__1[inline928]
    t809 = inline929
    var inline925 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t809)
    _goml_runtime_core_string_println(inline925)
    var t810 int32
    var inline922 int = 1
    var inline923 int32 = s__1[inline922]
    t810 = inline923
    var inline919 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t810)
    _goml_runtime_core_string_println(inline919)
    var t811 int32
    var inline916 int = 2
    var inline917 int32 = s__1[inline916]
    t811 = inline917
    var inline913 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t811)
    _goml_runtime_core_string_println(inline913)
    var t__2 []int32
    var inline909 int = 1
    var inline910 int = 3
    var inline911 []int32 = s__1[inline909:inline910]
    t__2 = inline911
    var t812 int
    var inline907 int = len(t__2)
    t812 = inline907
    var inline904 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t812)
    _goml_runtime_core_string_println(inline904)
    var t813 int32
    var inline901 int = 0
    var inline902 int32 = t__2[inline901]
    t813 = inline902
    var inline898 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t813)
    _goml_runtime_core_string_println(inline898)
    var t814 int32
    var inline895 int = 1
    var inline896 int32 = t__2[inline895]
    t814 = inline896
    var inline892 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t814)
    _goml_runtime_core_string_println(inline892)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32() *_goml_vec_int32 {
    var t817 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t817
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline956 int64 = int64(int(self__404))
    var inline957 string = signed_decimal_string(inline956)
    return inline957
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline959 int64 = int64(int32(self__407))
    var inline960 string = signed_decimal_string(inline959)
    return inline960
}

func signed_decimal_string(value__214 int64) string {
    var t856 bool = value__214 < 0
    if t856 {
        var t857 uint64 = uint64(int64(value__214))
        var t858 uint64 = 0 - t857
        var t859 string = decimal_string(t858)
        var t860 string = "-" + t859
        return t860
    } else {
        var t861 uint64 = uint64(int64(value__214))
        var t862 string = decimal_string(t861)
        return t862
    }
}

func decimal_string(value__208 uint64) string {
    var t885 bool = value__208 == 0
    if t885 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop878:
        for {
            var t879 bool = remaining__210 > 0
            if t879 {
                var t880_rhs uint64 = 10
                var t880 uint64 = remaining__210 % t880_rhs
                var t881 uint8 = uint8(uint64(t880))
                var t882 uint8 = t881 + 48
                vec_push__Vec_5uint8(reversed__209, t882)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t883 uint64 = compound_old353 / compound_value354
                remaining__210 = t883
                continue
            } else {
                break Loop_loop878
            }
        }
        var t867 int
        var inline986 int = vec_len__Vec_5uint8(reversed__209)
        t867 = inline986
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t867)
        var offset__212 int = 0
        Loop_loop869:
        for {
            var t870 int
            var inline984 int = vec_len__Vec_5uint8(reversed__209)
            t870 = inline984
            var t871 bool = offset__212 < t870
            if t871 {
                var t872 int
                var inline982 int = vec_len__Vec_5uint8(reversed__209)
                t872 = inline982
                var t873 int = t872 - offset__212
                var t874 int = t873 - 1
                var t875 uint8 = vec_get__Vec_5uint8(reversed__209, t874)
                vec_push__Vec_5uint8(bytes__211, t875)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t876 int = compound_old358 + compound_value359
                offset__212 = t876
                continue
            } else {
                break Loop_loop869
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
