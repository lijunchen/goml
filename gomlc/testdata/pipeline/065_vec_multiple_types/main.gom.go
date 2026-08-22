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

type _goml_vec_bool struct {
    items []bool
}

func vec_new__Vec_4bool() *_goml_vec_bool {
    return &_goml_vec_bool{
        items: nil,
    }
}

func vec_push__Vec_4bool(vec *_goml_vec_bool, elem bool) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_4bool(vec *_goml_vec_bool, index int) bool {
    return vec.items[index]
}

func vec_len__Vec_4bool(vec *_goml_vec_bool) int {
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
    var vi__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32()
    var inline949 int32 = 42
    vec_push__Vec_5int32(vi__0, inline949)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int
    var inline947 int = vec_len__Vec_5int32(vi__0)
    len_i__2 = inline947
    var vs__3 *_goml_vec_string
    var inline945 *_goml_vec_string = vec_new__Vec_6string()
    vs__3 = inline945
    var inline942 string = "hello"
    vec_push__Vec_6string(vs__3, inline942)
    var inline939 string = "world"
    vec_push__Vec_6string(vs__3, inline939)
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int
    var inline937 int = vec_len__Vec_6string(vs__3)
    len_s__5 = inline937
    var vb__6 *_goml_vec_bool
    var inline935 *_goml_vec_bool = vec_new__Vec_4bool()
    vb__6 = inline935
    var inline932 bool = true
    vec_push__Vec_4bool(vb__6, inline932)
    var inline929 bool = false
    vec_push__Vec_4bool(vb__6, inline929)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int
    var inline927 int = vec_len__Vec_4bool(vb__6)
    len_b__8 = inline927
    var t808 string
    var inline925 string = __goml_builtin_int32_to_string(val_i__1)
    t808 = inline925
    var inline922 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline922)
    var t809 string
    var inline920 string = __goml_builtin_int_to_string(len_i__2)
    t809 = inline920
    var inline917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t809)
    _goml_runtime_core_string_println(inline917)
    var inline914 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(val_s__4)
    _goml_runtime_core_string_println(inline914)
    var t810 string
    var inline912 string = __goml_builtin_int_to_string(len_s__5)
    t810 = inline912
    var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline909)
    var t811 string
    var inline907 string = _goml_runtime_core_bool_to_string(val_b__7)
    t811 = inline907
    var inline904 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t811)
    _goml_runtime_core_string_println(inline904)
    var t812 string
    var inline902 string = __goml_builtin_int_to_string(len_b__8)
    t812 = inline902
    var inline899 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline899)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32() *_goml_vec_int32 {
    var t815 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t815
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t853 int64 = int64(int32(value__225))
    var inline959 bool = t853 < 0
    if inline959 {
        var inline960 uint64 = uint64(int64(t853))
        var inline961 uint64 = 0 - inline960
        var inline962 string = decimal_string(inline961)
        var inline963 string = "-" + inline962
        return inline963
    } else {
        var inline964 uint64 = uint64(int64(t853))
        var inline965 string = decimal_string(inline964)
        return inline965
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t857 int64 = int64(int(value__222))
    var inline967 bool = t857 < 0
    if inline967 {
        var inline968 uint64 = uint64(int64(t857))
        var inline969 uint64 = 0 - inline968
        var inline970 string = decimal_string(inline969)
        var inline971 string = "-" + inline970
        return inline971
    } else {
        var inline972 uint64 = uint64(int64(t857))
        var inline973 string = decimal_string(inline972)
        return inline973
    }
}

func decimal_string(value__208 uint64) string {
    var t892 bool = value__208 == 0
    if t892 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop885:
        for {
            var t886 bool = remaining__210 > 0
            if t886 {
                var t887_rhs uint64 = 10
                var t887 uint64 = remaining__210 % t887_rhs
                var t888 uint8 = uint8(uint64(t887))
                var t889 uint8 = t888 + 48
                vec_push__Vec_5uint8(reversed__209, t889)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t890 uint64 = compound_old353 / compound_value354
                remaining__210 = t890
                continue
            } else {
                break Loop_loop885
            }
        }
        var t874 int
        var inline983 int = vec_len__Vec_5uint8(reversed__209)
        t874 = inline983
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t874)
        var offset__212 int = 0
        Loop_loop876:
        for {
            var t877 int
            var inline981 int = vec_len__Vec_5uint8(reversed__209)
            t877 = inline981
            var t878 bool = offset__212 < t877
            if t878 {
                var t879 int
                var inline979 int = vec_len__Vec_5uint8(reversed__209)
                t879 = inline979
                var t880 int = t879 - offset__212
                var t881 int = t880 - 1
                var t882 uint8 = vec_get__Vec_5uint8(reversed__209, t881)
                vec_push__Vec_5uint8(bytes__211, t882)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t883 int = compound_old358 + compound_value359
                offset__212 = t883
                continue
            } else {
                break Loop_loop876
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
