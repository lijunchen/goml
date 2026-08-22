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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
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

func sum_to(limit__0 int32) int32 {
    var acc__1 *ref_int32_x
    var inline929 int32 = 0
    var inline930 *ref_int32_x = ref__Ref_5int32(inline929)
    acc__1 = inline930
    var i__2 *ref_int32_x
    var inline926 int32 = 0
    var inline927 *ref_int32_x = ref__Ref_5int32(inline926)
    i__2 = inline927
    Loop_loop810:
    for {
        var t811 int32
        var inline922 int32 = ref_get__Ref_5int32(i__2)
        t811 = inline922
        var t812 bool = t811 < limit__0
        if t812 {
            var current__3 int32
            var inline920 int32 = ref_get__Ref_5int32(i__2)
            current__3 = inline920
            var t813 int32
            var inline918 int32 = ref_get__Ref_5int32(acc__1)
            t813 = inline918
            var t814 int32 = t813 + current__3
            ref_set__Ref_5int32(acc__1, t814)
            var t815 int32 = current__3 + 1
            ref_set__Ref_5int32(i__2, t815)
            continue
        } else {
            break Loop_loop810
        }
    }
    var inline924 int32 = ref_get__Ref_5int32(acc__1)
    return inline924
}

func sum_even(limit__4 int32) int32 {
    var acc__5 *ref_int32_x
    var inline954 int32 = 0
    var inline955 *ref_int32_x = ref__Ref_5int32(inline954)
    acc__5 = inline955
    var i__6 *ref_int32_x
    var inline951 int32 = 0
    var inline952 *ref_int32_x = ref__Ref_5int32(inline951)
    i__6 = inline952
    var is_even__7 *ref_bool_x
    var inline948 bool = true
    var inline949 *ref_bool_x = ref__Ref_4bool(inline948)
    is_even__7 = inline949
    Loop_loop820:
    for {
        var t821 int32
        var inline944 int32 = ref_get__Ref_5int32(i__6)
        t821 = inline944
        var t822 bool = t821 < limit__4
        if t822 {
            var current__8 int32
            var inline942 int32 = ref_get__Ref_5int32(i__6)
            current__8 = inline942
            var t823 int32 = current__8 + 1
            ref_set__Ref_5int32(i__6, t823)
            var add_now__9 bool
            var inline938 bool = ref_get__Ref_4bool(is_even__7)
            add_now__9 = inline938
            var t824 bool = !add_now__9
            ref_set__Ref_4bool(is_even__7, t824)
            if add_now__9 {
                var t826 int32
                var inline934 int32 = ref_get__Ref_5int32(acc__5)
                t826 = inline934
                var t827 int32 = t826 + current__8
                ref_set__Ref_5int32(acc__5, t827)
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop820
        }
    }
    var inline946 int32 = ref_get__Ref_5int32(acc__5)
    return inline946
}

func main0() struct{} {
    var first__10 int32 = sum_to(5)
    var evens__11 int32 = sum_even(6)
    var inline967 string = "sum_to(5)="
    var inline968 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline967)
    _goml_runtime_core_string_print(inline968)
    var inline964 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(first__10)
    _goml_runtime_core_string_println(inline964)
    var inline960 string = "sum_even(6)="
    var inline961 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline960)
    _goml_runtime_core_string_print(inline961)
    var inline957 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(evens__11)
    _goml_runtime_core_string_println(inline957)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline974 int64 = int64(int32(self__407))
    var inline975 string = signed_decimal_string(inline974)
    return inline975
}

func signed_decimal_string(value__214 int64) string {
    var t865 bool = value__214 < 0
    if t865 {
        var t866 uint64 = uint64(int64(value__214))
        var t867 uint64 = 0 - t866
        var t868 string = decimal_string(t867)
        var t869 string = "-" + t868
        return t869
    } else {
        var t870 uint64 = uint64(int64(value__214))
        var t871 string = decimal_string(t870)
        return t871
    }
}

func decimal_string(value__208 uint64) string {
    var t894 bool = value__208 == 0
    if t894 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop887:
        for {
            var t888 bool = remaining__210 > 0
            if t888 {
                var t889_rhs uint64 = 10
                var t889 uint64 = remaining__210 % t889_rhs
                var t890 uint8 = uint8(uint64(t889))
                var t891 uint8 = t890 + 48
                vec_push__Vec_5uint8(reversed__209, t891)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t892 uint64 = compound_old353 / compound_value354
                remaining__210 = t892
                continue
            } else {
                break Loop_loop887
            }
        }
        var t876 int
        var inline993 int = vec_len__Vec_5uint8(reversed__209)
        t876 = inline993
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t876)
        var offset__212 int = 0
        Loop_loop878:
        for {
            var t879 int
            var inline991 int = vec_len__Vec_5uint8(reversed__209)
            t879 = inline991
            var t880 bool = offset__212 < t879
            if t880 {
                var t881 int
                var inline989 int = vec_len__Vec_5uint8(reversed__209)
                t881 = inline989
                var t882 int = t881 - offset__212
                var t883 int = t882 - 1
                var t884 uint8 = vec_get__Vec_5uint8(reversed__209, t883)
                vec_push__Vec_5uint8(bytes__211, t884)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t885 int = compound_old358 + compound_value359
                offset__212 = t885
                continue
            } else {
                break Loop_loop878
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
