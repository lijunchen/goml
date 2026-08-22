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

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func unwrap_or_negative(value__0 Option__isize) int {
    switch value__0._tag {
    case 1:
        var x797 int = value__0._v1_0
        return x797
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline943 int = 0
    var inline944 *ref_int_x = ref__Ref_3int(inline943)
    counter__3 = inline944
    var jp825 int
    Loop_loop_expr826:
    for {
        var current__4 int
        var inline941 int = ref_get__Ref_3int(counter__3)
        current__4 = inline941
        var t829 bool = current__4 >= limit__2
        if t829 {
            jp825 = current__4
            break Loop_loop_expr826
        } else {
            var t828 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t828)
            continue
        }
    }
    return jp825
}

func loop_option(value__5 Option__isize) int {
    var jp833 int
    switch value__5._tag {
    case 1:
        var x803 int = value__5._v1_0
        jp833 = x803
        return jp833
    default:
        jp833 = -2
        return jp833
    }
}

func nested_loop_value() int {
    var jp839 int
    jp839 = 7
    return jp839
}

func main0() struct{} {
    println__T_string("C:\\tmp\\\"quoted\\\"")
    var t853 string = "" + "}"
    var inline987 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t853)
    _goml_runtime_core_string_println(inline987)
    var t854 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 11,
    }
    var t855 int = unwrap_or_negative(t854)
    var t856 string
    var inline985 string = __goml_builtin_int_to_string(t855)
    t856 = inline985
    var inline982 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t856)
    _goml_runtime_core_string_println(inline982)
    var t857 int
    t857 = -1
    var t858 string
    var inline976 string = __goml_builtin_int_to_string(t857)
    t858 = inline976
    var inline973 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t858)
    _goml_runtime_core_string_println(inline973)
    var t859 int = count_to(4)
    var t860 string
    var inline971 string = __goml_builtin_int_to_string(t859)
    t860 = inline971
    var inline968 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t860)
    _goml_runtime_core_string_println(inline968)
    var t861 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 9,
    }
    var t862 int = loop_option(t861)
    var t863 string
    var inline966 string = __goml_builtin_int_to_string(t862)
    t863 = inline966
    var inline963 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t863)
    _goml_runtime_core_string_println(inline963)
    var t864 int = loop_option(Option__isize{
        _tag: 0,
    })
    var t865 string
    var inline961 string = __goml_builtin_int_to_string(t864)
    t865 = inline961
    var inline958 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t865)
    _goml_runtime_core_string_println(inline958)
    var t866 int = nested_loop_value()
    var t867 string
    var inline956 string = __goml_builtin_int_to_string(t866)
    t867 = inline956
    var inline953 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t867)
    _goml_runtime_core_string_println(inline953)
    var t868 bool
    var inline951 string = "C:\\tmp"
    switch inline951 {
    case "C:\\tmp":
        t868 = true
    default:
        t868 = false
    }
    var t869 string
    var inline949 string = _goml_runtime_core_bool_to_string(t868)
    t869 = inline949
    var inline946 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t869)
    _goml_runtime_core_string_println(inline946)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t879 string
    t879 = value__1
    _goml_runtime_core_string_println(t879)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t891 int64 = int64(int(value__222))
    var inline994 bool = t891 < 0
    if inline994 {
        var inline995 uint64 = uint64(int64(t891))
        var inline996 uint64 = 0 - inline995
        var inline997 string = decimal_string(inline996)
        var inline998 string = "-" + inline997
        return inline998
    } else {
        var inline999 uint64 = uint64(int64(t891))
        var inline1000 string = decimal_string(inline999)
        return inline1000
    }
}

func decimal_string(value__208 uint64) string {
    var t926 bool = value__208 == 0
    if t926 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop919:
        for {
            var t920 bool = remaining__210 > 0
            if t920 {
                var t921_rhs uint64 = 10
                var t921 uint64 = remaining__210 % t921_rhs
                var t922 uint8 = uint8(uint64(t921))
                var t923 uint8 = t922 + 48
                vec_push__Vec_5uint8(reversed__209, t923)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t924 uint64 = compound_old353 / compound_value354
                remaining__210 = t924
                continue
            } else {
                break Loop_loop919
            }
        }
        var t908 int
        var inline1010 int = vec_len__Vec_5uint8(reversed__209)
        t908 = inline1010
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t908)
        var offset__212 int = 0
        Loop_loop910:
        for {
            var t911 int
            var inline1008 int = vec_len__Vec_5uint8(reversed__209)
            t911 = inline1008
            var t912 bool = offset__212 < t911
            if t912 {
                var t913 int
                var inline1006 int = vec_len__Vec_5uint8(reversed__209)
                t913 = inline1006
                var t914 int = t913 - offset__212
                var t915 int = t914 - 1
                var t916 uint8 = vec_get__Vec_5uint8(reversed__209, t915)
                vec_push__Vec_5uint8(bytes__211, t916)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t917 int = compound_old358 + compound_value359
                offset__212 = t917
                continue
            } else {
                break Loop_loop910
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
