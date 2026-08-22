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

func _goml_intrinsic_missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

func array_get__Array_4_3int(arr [4]int, index int) int {
    return arr[index]
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
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

type ref_Maybe_x struct {
    value Maybe
}

func ref__Ref_5Maybe(value Maybe) *ref_Maybe_x {
    return &ref_Maybe_x{
        value: value,
    }
}

func ref_get__Ref_5Maybe(reference *ref_Maybe_x) Maybe {
    return reference.value
}

func ref_set__Ref_5Maybe(reference *ref_Maybe_x, value Maybe) struct{} {
    reference.value = value
    return struct{}{}
}

func missing__int32(s string) int32 {
    _goml_intrinsic_missing(s)
    var ret int32
    return ret
}

func missing__string(s string) string {
    _goml_intrinsic_missing(s)
    var ret string
    return ret
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
    left int32
    right int32
}

type Ordering int32

type Maybe struct {
    _tag int32
    _v1_0 int32
}

type Either struct {
    _tag int32
    _v0_0 int32
    _v1_0 int32
}

func unwrap_either(value__0 Either) int32 {
    switch value__0._tag {
    case 0:
        var shared__2 int32 = value__0._v0_0
        var jp836 int32
        switch value__0._tag {
        case 0:
            jp836 = 0
        case 1:
            jp836 = 1
        default:
            panic("non-exhaustive match")
        }
        var t837 int32 = shared__2 + jp836
        return t837
    default:
        switch value__0._tag {
        case 1:
            var shared__2 int32 = value__0._v1_0
            var jp841 int32
            switch value__0._tag {
            case 0:
                jp841 = 0
            case 1:
                jp841 = 1
            default:
                panic("non-exhaustive match")
            }
            var t842 int32 = shared__2 + jp841
            return t842
        default:
            var t843 int32 = missing__int32("")
            return t843
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp855 string
    switch value__4._tag {
    case 1:
        var x802 int32 = value__4._v1_0
        var t896 bool = x802 == 0
        if t896 {
            jp855 = "small"
        } else {
            var t899 bool = x802 == 1
            if t899 {
                jp855 = "small"
            } else {
                var t902 bool = x802 >= 2
                if t902 {
                    var t905 bool = x802 <= 4
                    if t905 {
                        jp855 = "middle"
                    } else {
                        var t908 bool = x802 > 10
                        if t908 {
                            jp855 = "large"
                        } else {
                            jp855 = "other"
                        }
                    }
                } else {
                    var t911 bool = x802 > 10
                    if t911 {
                        jp855 = "large"
                    } else {
                        jp855 = "other"
                    }
                }
            }
        }
    default:
        jp855 = "none"
    }
    var t872 int = vec_len__Vec_5int32(numbers__5)
    var t873 bool = t872 == 0
    var jp857 string
    if t873 {
        jp857 = "empty"
    } else {
        var t876 int = vec_len__Vec_5int32(numbers__5)
        var t877 bool = t876 >= 1
        if t877 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t878 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t878]
            var t881 int
            var inline1017 int = len(tail__11)
            t881 = inline1017
            var t882 int32 = int32(int(t881))
            var t883 bool = first__10 == t882
            if t883 {
                jp857 = "balanced"
            } else {
                var t886 int = vec_len__Vec_5int32(numbers__5)
                var t887 bool = t886 >= 1
                if t887 {
                    jp857 = "nonempty"
                } else {
                    var t888 string = missing__string("")
                    jp857 = t888
                }
            }
        } else {
            var t891 int = vec_len__Vec_5int32(numbers__5)
            var t892 bool = t891 >= 1
            if t892 {
                jp857 = "nonempty"
            } else {
                var t893 string = missing__string("")
                jp857 = t893
            }
        }
    }
    var t864 int = len(view__6)
    var t865 bool = t864 >= 2
    var jp859 string
    if t865 {
        var first__13 int32 = view__6[0]
        var t866 int = len(view__6)
        var t867 int = t866 - 1
        var t868 int = t867 + 0
        var last__14 int32 = view__6[t868]
        var t871 bool = first__13 == last__14
        if t871 {
            jp859 = "same ends"
        } else {
            jp859 = "different ends"
        }
    } else {
        jp859 = "different ends"
    }
    var t860 string = jp855 + "/"
    var t861 string = t860 + jp857
    var t862 string = t861 + "/"
    var t863 string = t862 + jp859
    return t863
}

func main0() struct{} {
    var x807 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t913 int = array_get__Array_4_3int(values__18, 1)
    var t914 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t913, t914}
    var inline1056 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x807)
    _goml_runtime_core_string_println(inline1056)
    var t915 int = array_get__Array_2_3int(middle__20, 0)
    var t916 int = first__19 + t915
    var t917 int = t916 + last__21
    var inline1053 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t917)
    _goml_runtime_core_string_println(inline1053)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t918 int
    var inline1051 int = vec_len__Vec_5int32(numbers__22)
    t918 = inline1051
    var view__23 []int32 = numbers__22.items[0:t918]
    var t919 Maybe = Maybe{
        _tag: 1,
        _v1_0: 3,
    }
    var t920 string = describe(t919, numbers__22, view__23)
    var inline1048 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t920)
    _goml_runtime_core_string_println(inline1048)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t921 string = describe(Maybe{
        _tag: 0,
    }, empty__24, empty_view__25)
    var inline1045 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t921)
    _goml_runtime_core_string_println(inline1045)
    var t922 Maybe = Maybe{
        _tag: 1,
        _v1_0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline1043 *ref_Maybe_x = ref__Ref_5Maybe(t922)
    state__26 = inline1043
    Loop_loop936:
    for {
        var mtmp816 Maybe
        var inline1024 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp816 = inline1024
        switch mtmp816._tag {
        case 1:
            var x817 int32 = mtmp816._v1_0
            var inline1021 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x817)
            _goml_runtime_core_string_println(inline1021)
            ref_set__Ref_5Maybe(state__26, Maybe{
                _tag: 0,
            })
            continue
        default:
            break Loop_loop936
        }
    }
    var x822 int32 = 6
    var inline1026 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x822)
    _goml_runtime_core_string_println(inline1026)
    var jp926 int32
    var value__30 int32 = 5
    var jp931 int32
    var x826 int32 = 5
    jp931 = x826
    var t932 int32 = value__30 + jp931
    jp926 = t932
    var inline1040 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(jp926)
    _goml_runtime_core_string_println(inline1040)
    var t927 Either = Either{
        _tag: 1,
        _v1_0: 11,
    }
    var t928 int32 = unwrap_either(t927)
    var inline1037 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t928)
    _goml_runtime_core_string_println(inline1037)
    var t929 string
    var inline1032 rune = 98
    var inline1034 bool = inline1032 >= 97
    if inline1034 {
        var inline1035 bool = inline1032 <= 99
        if inline1035 {
            t929 = "abc"
        } else {
            t929 = "other"
        }
    } else {
        t929 = "other"
    }
    var inline1029 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t929)
    _goml_runtime_core_string_println(inline1029)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1064 int64 = int64(int32(self__407))
    var inline1065 string = signed_decimal_string(inline1064)
    return inline1065
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1067 int64 = int64(int(self__404))
    var inline1068 string = signed_decimal_string(inline1067)
    return inline1068
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t981 bool = value__214 < 0
    if t981 {
        var t982 uint64 = uint64(int64(value__214))
        var t983 uint64 = 0 - t982
        var t984 string = decimal_string(t983)
        var t985 string = "-" + t984
        return t985
    } else {
        var t986 uint64 = uint64(int64(value__214))
        var t987 string = decimal_string(t986)
        return t987
    }
}

func decimal_string(value__208 uint64) string {
    var t1010 bool = value__208 == 0
    if t1010 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1003:
        for {
            var t1004 bool = remaining__210 > 0
            if t1004 {
                var t1005_rhs uint64 = 10
                var t1005 uint64 = remaining__210 % t1005_rhs
                var t1006 uint8 = uint8(uint64(t1005))
                var t1007 uint8 = t1006 + 48
                vec_push__Vec_5uint8(reversed__209, t1007)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1008 uint64 = compound_old353 / compound_value354
                remaining__210 = t1008
                continue
            } else {
                break Loop_loop1003
            }
        }
        var t992 int
        var inline1094 int = vec_len__Vec_5uint8(reversed__209)
        t992 = inline1094
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t992)
        var offset__212 int = 0
        Loop_loop994:
        for {
            var t995 int
            var inline1092 int = vec_len__Vec_5uint8(reversed__209)
            t995 = inline1092
            var t996 bool = offset__212 < t995
            if t996 {
                var t997 int
                var inline1090 int = vec_len__Vec_5uint8(reversed__209)
                t997 = inline1090
                var t998 int = t997 - offset__212
                var t999 int = t998 - 1
                var t1000 uint8 = vec_get__Vec_5uint8(reversed__209, t999)
                vec_push__Vec_5uint8(bytes__211, t1000)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1001 int = compound_old358 + compound_value359
                offset__212 = t1001
                continue
            } else {
                break Loop_loop994
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
