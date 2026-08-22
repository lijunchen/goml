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

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

func vec_reserve__Vec_5int32(vec *_goml_vec_int32, additional int) struct{} {
    if additional < 0 {
        panic("negative vector capacity")
    }
    var length int = len(vec.items)
    var required int = length + additional
    if required < length {
        panic("vector capacity overflow")
    }
    if required > cap(vec.items) {
        var next_capacity int = cap(vec.items) * 2
        if next_capacity < required {
            next_capacity = required
        }
        var next_items []int32 = make([]int32, length, next_capacity)
        copy(next_items, vec.items)
        vec.items = next_items
    }
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

func print_values(values__0 *_goml_vec_int32) struct{} {
    var t813 int
    var inline904 int = vec_len__Vec_5int32(values__0)
    t813 = inline904
    var inline901 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t813)
    _goml_runtime_core_string_println(inline901)
    var for_limit798 int = vec_len__Vec_5int32(values__0)
    var for_index799 int = 0
    Loop_loop815:
    for {
        var t816 bool = for_index799 < for_limit798
        if t816 {
            var for_item800 int32 = vec_get__Vec_5int32(values__0, for_index799)
            var t817 int = for_index799 + 1
            for_index799 = t817
            var inline898 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(for_item800)
            _goml_runtime_core_string_println(inline898)
            continue
        } else {
            break Loop_loop815
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var values__2 *_goml_vec_int32
    var inline925 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__2 = inline925
    var inline922 int32 = 1
    vec_push__Vec_5int32(values__2, inline922)
    var inline919 int32 = 2
    vec_push__Vec_5int32(values__2, inline919)
    var inline916 int32 = 3
    vec_push__Vec_5int32(values__2, inline916)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__i32(values__2, values__2)
    print_values(values__2)
    var aliased__3 *_goml_vec_int32
    var inline914 *_goml_vec_int32 = vec_new__Vec_5int32()
    aliased__3 = inline914
    var inline911 int32 = 4
    vec_push__Vec_5int32(aliased__3, inline911)
    var inline908 int32 = 5
    vec_push__Vec_5int32(aliased__3, inline908)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__i32(aliased__3, aliased__3)
    print_values(aliased__3)
    var empty__5 *_goml_vec_int32
    var inline906 *_goml_vec_int32 = vec_new__Vec_5int32()
    empty__5 = inline906
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__i32(empty__5, empty__5)
    print_values(empty__5)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_extend____T__i32(self__560 *_goml_vec_int32, other__561 *_goml_vec_int32) struct{} {
    var len__562 int
    var inline935 int = vec_len__Vec_5int32(other__561)
    len__562 = inline935
    vec_reserve__Vec_5int32(self__560, len__562)
    var index__563 int = 0
    Loop_loop837:
    for {
        var t838 bool = index__563 < len__562
        if t838 {
            var t839 int32 = vec_get__Vec_5int32(other__561, index__563)
            vec_push__Vec_5int32(self__560, t839)
            var compound_old638 int = index__563
            var compound_value639 int = 1
            var t840 int = compound_old638 + compound_value639
            index__563 = t840
            continue
        } else {
            break Loop_loop837
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline937 int64 = int64(int(self__404))
    var inline938 string = signed_decimal_string(inline937)
    return inline938
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline940 int64 = int64(int32(self__407))
    var inline941 string = signed_decimal_string(inline940)
    return inline941
}

func signed_decimal_string(value__214 int64) string {
    var t862 bool = value__214 < 0
    if t862 {
        var t863 uint64 = uint64(int64(value__214))
        var t864 uint64 = 0 - t863
        var t865 string = decimal_string(t864)
        var t866 string = "-" + t865
        return t866
    } else {
        var t867 uint64 = uint64(int64(value__214))
        var t868 string = decimal_string(t867)
        return t868
    }
}

func decimal_string(value__208 uint64) string {
    var t891 bool = value__208 == 0
    if t891 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop884:
        for {
            var t885 bool = remaining__210 > 0
            if t885 {
                var t886_rhs uint64 = 10
                var t886 uint64 = remaining__210 % t886_rhs
                var t887 uint8 = uint8(uint64(t886))
                var t888 uint8 = t887 + 48
                vec_push__Vec_5uint8(reversed__209, t888)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t889 uint64 = compound_old353 / compound_value354
                remaining__210 = t889
                continue
            } else {
                break Loop_loop884
            }
        }
        var t873 int
        var inline967 int = vec_len__Vec_5uint8(reversed__209)
        t873 = inline967
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t873)
        var offset__212 int = 0
        Loop_loop875:
        for {
            var t876 int
            var inline965 int = vec_len__Vec_5uint8(reversed__209)
            t876 = inline965
            var t877 bool = offset__212 < t876
            if t877 {
                var t878 int
                var inline963 int = vec_len__Vec_5uint8(reversed__209)
                t878 = inline963
                var t879 int = t878 - offset__212
                var t880 int = t879 - 1
                var t881 uint8 = vec_get__Vec_5uint8(reversed__209, t880)
                vec_push__Vec_5uint8(bytes__211, t881)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t882 int = compound_old358 + compound_value359
                offset__212 = t882
                continue
            } else {
                break Loop_loop875
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
