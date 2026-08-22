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

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
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
    var inline900 string = "if"
    var inline901 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline900)
    _goml_runtime_core_string_println(inline901)
    var mtmp797 int = 1
    switch mtmp797 {
    case 1:
        var inline904 string = "match"
        var inline905 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline904)
        _goml_runtime_core_string_println(inline905)
    default:
    }
    var index__0 *ref_int_x
    var inline933 int = 0
    var inline934 *ref_int_x = ref__Ref_3int(inline933)
    index__0 = inline934
    Loop_loop818:
    for {
        var t819 int
        var inline919 int = ref_get__Ref_3int(index__0)
        t819 = inline919
        var t820 bool = t819 < 2
        if t820 {
            var t821 int
            var inline917 int = ref_get__Ref_3int(index__0)
            t821 = inline917
            var t822 string
            var inline915 string = __goml_builtin_int_to_string(t821)
            t822 = inline915
            var inline912 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
            _goml_runtime_core_string_println(inline912)
            var t823 int
            var inline910 int = ref_get__Ref_3int(index__0)
            t823 = inline910
            var t824 int = t823 + 1
            ref_set__Ref_3int(index__0, t824)
            continue
        } else {
            break Loop_loop818
        }
    }
    var values__1 *_goml_vec_string
    var inline931 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline931
    var inline928 string = "for"
    vec_push__Vec_6string(values__1, inline928)
    var for_limit803 int = vec_len__Vec_6string(values__1)
    var for_index804 int = 0
    Loop_loop814:
    for {
        var t815 bool = for_index804 < for_limit803
        if t815 {
            var for_item805 string = vec_get__Vec_6string(values__1, for_index804)
            var t816 int = for_index804 + 1
            for_index804 = t816
            var inline921 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item805)
            _goml_runtime_core_string_println(inline921)
            continue
        } else {
            break Loop_loop814
        }
    }
    var inline924 string = "done"
    var inline925 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline924)
    _goml_runtime_core_string_println(inline925)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t852 int64 = int64(int(value__222))
    var inline940 bool = t852 < 0
    if inline940 {
        var inline941 uint64 = uint64(int64(t852))
        var inline942 uint64 = 0 - inline941
        var inline943 string = decimal_string(inline942)
        var inline944 string = "-" + inline943
        return inline944
    } else {
        var inline945 uint64 = uint64(int64(t852))
        var inline946 string = decimal_string(inline945)
        return inline946
    }
}

func decimal_string(value__208 uint64) string {
    var t887 bool = value__208 == 0
    if t887 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop880:
        for {
            var t881 bool = remaining__210 > 0
            if t881 {
                var t882_rhs uint64 = 10
                var t882 uint64 = remaining__210 % t882_rhs
                var t883 uint8 = uint8(uint64(t882))
                var t884 uint8 = t883 + 48
                vec_push__Vec_5uint8(reversed__209, t884)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t885 uint64 = compound_old353 / compound_value354
                remaining__210 = t885
                continue
            } else {
                break Loop_loop880
            }
        }
        var t869 int
        var inline956 int = vec_len__Vec_5uint8(reversed__209)
        t869 = inline956
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t869)
        var offset__212 int = 0
        Loop_loop871:
        for {
            var t872 int
            var inline954 int = vec_len__Vec_5uint8(reversed__209)
            t872 = inline954
            var t873 bool = offset__212 < t872
            if t873 {
                var t874 int
                var inline952 int = vec_len__Vec_5uint8(reversed__209)
                t874 = inline952
                var t875 int = t874 - offset__212
                var t876 int = t875 - 1
                var t877 uint8 = vec_get__Vec_5uint8(reversed__209, t876)
                vec_push__Vec_5uint8(bytes__211, t877)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t878 int = compound_old358 + compound_value359
                offset__212 = t878
                continue
            } else {
                break Loop_loop871
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
