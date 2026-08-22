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
    var v__0 *_goml_vec_int32
    var inline906 *_goml_vec_int32 = vec_new__Vec_5int32()
    v__0 = inline906
    var inline903 int32 = 10
    vec_push__Vec_5int32(v__0, inline903)
    var inline900 int32 = 20
    vec_push__Vec_5int32(v__0, inline900)
    var inline897 int32 = 30
    vec_push__Vec_5int32(v__0, inline897)
    var first__1 int32 = vec_get__Vec_5int32(v__0, 0)
    var second__2 int32 = vec_get__Vec_5int32(v__0, 1)
    var third__3 int32 = vec_get__Vec_5int32(v__0, 2)
    var len__4 int
    var inline895 int = vec_len__Vec_5int32(v__0)
    len__4 = inline895
    var t804 string
    var inline893 string = __goml_builtin_int32_to_string(first__1)
    t804 = inline893
    var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t804)
    _goml_runtime_core_string_println(inline890)
    var t805 string
    var inline888 string = __goml_builtin_int32_to_string(second__2)
    t805 = inline888
    var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t805)
    _goml_runtime_core_string_println(inline885)
    var t806 string
    var inline883 string = __goml_builtin_int32_to_string(third__3)
    t806 = inline883
    var inline880 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline880)
    var t807 string
    var inline878 string = __goml_builtin_int_to_string(len__4)
    t807 = inline878
    var inline875 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline875)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t829 int64 = int64(int32(value__225))
    var inline915 bool = t829 < 0
    if inline915 {
        var inline916 uint64 = uint64(int64(t829))
        var inline917 uint64 = 0 - inline916
        var inline918 string = decimal_string(inline917)
        var inline919 string = "-" + inline918
        return inline919
    } else {
        var inline920 uint64 = uint64(int64(t829))
        var inline921 string = decimal_string(inline920)
        return inline921
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t833 int64 = int64(int(value__222))
    var inline923 bool = t833 < 0
    if inline923 {
        var inline924 uint64 = uint64(int64(t833))
        var inline925 uint64 = 0 - inline924
        var inline926 string = decimal_string(inline925)
        var inline927 string = "-" + inline926
        return inline927
    } else {
        var inline928 uint64 = uint64(int64(t833))
        var inline929 string = decimal_string(inline928)
        return inline929
    }
}

func decimal_string(value__208 uint64) string {
    var t868 bool = value__208 == 0
    if t868 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop861:
        for {
            var t862 bool = remaining__210 > 0
            if t862 {
                var t863_rhs uint64 = 10
                var t863 uint64 = remaining__210 % t863_rhs
                var t864 uint8 = uint8(uint64(t863))
                var t865 uint8 = t864 + 48
                vec_push__Vec_5uint8(reversed__209, t865)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t866 uint64 = compound_old353 / compound_value354
                remaining__210 = t866
                continue
            } else {
                break Loop_loop861
            }
        }
        var t850 int
        var inline939 int = vec_len__Vec_5uint8(reversed__209)
        t850 = inline939
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t850)
        var offset__212 int = 0
        Loop_loop852:
        for {
            var t853 int
            var inline937 int = vec_len__Vec_5uint8(reversed__209)
            t853 = inline937
            var t854 bool = offset__212 < t853
            if t854 {
                var t855 int
                var inline935 int = vec_len__Vec_5uint8(reversed__209)
                t855 = inline935
                var t856 int = t855 - offset__212
                var t857 int = t856 - 1
                var t858 uint8 = vec_get__Vec_5uint8(reversed__209, t857)
                vec_push__Vec_5uint8(bytes__211, t858)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t859 int = compound_old358 + compound_value359
                offset__212 = t859
                continue
            } else {
                break Loop_loop852
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
