package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
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

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func show_scalar(value__0 uint32) struct{} {
    var mtmp796 Option__char
    var inline898 Option__char = __goml_builtin_char_from_uint32(value__0)
    mtmp796 = inline898
    switch mtmp796._tag {
    case 0:
        var inline891 string = "none"
        var inline892 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline891)
        _goml_runtime_core_string_println(inline892)
        return struct{}{}
    case 1:
        var x797 rune = mtmp796._v1_0
        var t811 uint32 = uint32(rune(x797))
        var inline895 string = _goml_m_trait__impl_i_ToString_i_u32_i_to__string(t811)
        _goml_runtime_core_string_println(inline895)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    show_scalar(0)
    show_scalar(65)
    show_scalar(55295)
    show_scalar(55296)
    var inline936 uint32 = 57343
    var inline937 Option__char = char_from_u32(inline936)
    switch inline937._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline939 rune = inline937._v1_0
        var inline941 uint32 = uint32(rune(inline939))
        println__T_u32(inline941)
    default:
        panic("non-exhaustive match")
    }
    var inline928 uint32 = 57344
    var inline929 Option__char = char_from_u32(inline928)
    switch inline929._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline931 rune = inline929._v1_0
        var inline933 uint32 = uint32(rune(inline931))
        println__T_u32(inline933)
    default:
        panic("non-exhaustive match")
    }
    var inline920 uint32 = 1114111
    var inline921 Option__char = char_from_u32(inline920)
    switch inline921._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline923 rune = inline921._v1_0
        var inline925 uint32 = uint32(rune(inline923))
        println__T_u32(inline925)
    default:
        panic("non-exhaustive match")
    }
    var inline912 uint32 = 1114112
    var inline913 Option__char = char_from_u32(inline912)
    switch inline913._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline915 rune = inline913._v1_0
        var inline917 uint32 = uint32(rune(inline915))
        println__T_u32(inline917)
    default:
        panic("non-exhaustive match")
    }
    var mtmp806 Option__char
    var inline909 uint32 = 128512
    var inline910 Option__char = __goml_builtin_char_from_uint32(inline909)
    mtmp806 = inline910
    switch mtmp806._tag {
    case 0:
        var inline900 string = "none"
        var inline901 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline900)
        _goml_runtime_core_string_println(inline901)
        return struct{}{}
    case 1:
        var x807 rune = mtmp806._v1_0
        var t816 string
        var inline907 string = char_to_string(x807)
        t816 = inline907
        var inline904 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t816)
        _goml_runtime_core_string_println(inline904)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func char_from_u32(value__2 uint32) Option__char {
    var inline944 bool = utf8_valid_scalar(value__2)
    if inline944 {
        var inline945 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline946 rune = inline945._1
        var inline948 Option__char = Option__char{
            _tag: 1,
            _v1_0: inline946,
        }
        return inline948
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t822 string
    t822 = value__1
    _goml_runtime_core_string_println(t822)
    return struct{}{}
}

func println__T_u32(value__1 uint32) struct{} {
    var t825 string
    var inline951 string = __goml_builtin_uint32_to_string(value__1)
    t825 = inline951
    _goml_runtime_core_string_println(t825)
    return struct{}{}
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t834 bool
    var inline958 bool = value__283 <= 1114111
    if inline958 {
        var inline959 bool = value__283 >= 55296
        var inline961 bool
        if inline959 {
            var inline963 bool = value__283 <= 57343
            inline961 = inline963
        } else {
            inline961 = false
        }
        var inline962 bool = !inline961
        t834 = inline962
    } else {
        t834 = false
    }
    if t834 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t835 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t835
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_u32_i_to__string(self__411 uint32) string {
    var inline965 uint64 = uint64(uint32(self__411))
    var inline966 string = decimal_string(inline965)
    return inline966
}

func char_to_string(value__282 rune) string {
    var t845 uint32 = uint32(rune(value__282))
    var t846 bool
    var inline968 bool = t845 <= 1114111
    if inline968 {
        var inline969 bool = t845 >= 55296
        var inline971 bool
        if inline969 {
            var inline973 bool = t845 <= 57343
            inline971 = inline973
        } else {
            inline971 = false
        }
        var inline972 bool = !inline971
        t846 = inline972
    } else {
        t846 = false
    }
    if t846 {
        var t847 string = _goml_runtime_core_char_to_string(value__282)
        return t847
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t852 bool = value__257 <= 1114111
    if t852 {
        var t856 bool = value__257 >= 55296
        var jp854 bool
        if t856 {
            var t857 bool = value__257 <= 57343
            jp854 = t857
        } else {
            jp854 = false
        }
        var t855 bool = !jp854
        return t855
    } else {
        return false
    }
}

func __goml_builtin_uint32_to_string(value__230 uint32) string {
    var t860 uint64 = uint64(uint32(value__230))
    var t861 string = decimal_string(t860)
    return t861
}

func decimal_string(value__208 uint64) string {
    var t884 bool = value__208 == 0
    if t884 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop877:
        for {
            var t878 bool = remaining__210 > 0
            if t878 {
                var t879_rhs uint64 = 10
                var t879 uint64 = remaining__210 % t879_rhs
                var t880 uint8 = uint8(uint64(t879))
                var t881 uint8 = t880 + 48
                vec_push__Vec_5uint8(reversed__209, t881)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t882 uint64 = compound_old353 / compound_value354
                remaining__210 = t882
                continue
            } else {
                break Loop_loop877
            }
        }
        var t866 int
        var inline983 int = vec_len__Vec_5uint8(reversed__209)
        t866 = inline983
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t866)
        var offset__212 int = 0
        Loop_loop868:
        for {
            var t869 int
            var inline981 int = vec_len__Vec_5uint8(reversed__209)
            t869 = inline981
            var t870 bool = offset__212 < t869
            if t870 {
                var t871 int
                var inline979 int = vec_len__Vec_5uint8(reversed__209)
                t871 = inline979
                var t872 int = t871 - offset__212
                var t873 int = t872 - 1
                var t874 uint8 = vec_get__Vec_5uint8(reversed__209, t873)
                vec_push__Vec_5uint8(bytes__211, t874)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t875 int = compound_old358 + compound_value359
                offset__212 = t875
                continue
            } else {
                break Loop_loop868
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
