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

type Key struct {
    _tag int32
    _v1_0 int32
    _v1_1 int32
}

type Message__string interface {
    isMessage__string()
}

type Quit struct {}

func (_ Quit) isMessage__string() {}

type Write struct {
    _0 string
}

func (_ Write) isMessage__string() {}

type Move struct {
    _0 int32
    _1 int32
    _2 string
}

func (_ Move) isMessage__string() {}

func _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(self__10 Key, other__11 Key) bool {
    switch other__11._tag {
    case 0:
        switch self__10._tag {
        case 0:
            return true
        default:
            return false
        }
    case 1:
        var x803 int32 = other__11._v1_0
        var x804 int32 = other__11._v1_1
        switch self__10._tag {
        case 1:
            var x807 int32 = self__10._v1_0
            var x808 int32 = self__10._v1_1
            var jp860 bool
            var inline955 bool = x807 == x803
            jp860 = inline955
            if jp860 {
                var inline957 bool = x808 == x804
                return inline957
            } else {
                return false
            }
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__22 int32 = 3
    var t873 int32
    var inline990 int32 = 4
    var inline993 int32 = x__22 + inline990
    t873 = inline993
    var inline986 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t873)
    _goml_runtime_core_string_println(inline986)
    var t874 string
    var inline983 string = "north"
    t874 = inline983
    var inline978 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t874)
    _goml_runtime_core_string_println(inline978)
    var t876 string
    var inline965 int32 = 1
    var inline966 int32 = 2
    var inline969 string = "Key::Point { " + "x: "
    var inline970 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(inline965)
    var inline971 string = inline969 + inline970
    var inline972 string = inline971 + ", "
    var inline973 string = inline972 + "y: "
    var inline974 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(inline966)
    var inline975 string = inline973 + inline974
    var inline976 string = inline975 + " }"
    t876 = inline976
    var inline962 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t876)
    _goml_runtime_core_string_println(inline962)
    var t877 Key = Key{
        _tag: 1,
        _v1_0: 1,
        _v1_1: 2,
    }
    var t878 Key = Key{
        _tag: 1,
        _v1_0: 1,
        _v1_1: 2,
    }
    var t879 bool = _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(t877, t878)
    var inline959 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t879)
    _goml_runtime_core_string_println(inline959)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline995 int64 = int64(int32(self__407))
    var inline996 string = signed_decimal_string(inline995)
    return inline996
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t906 string = _goml_runtime_core_bool_to_string(self__401)
    return t906
}

func signed_decimal_string(value__214 int64) string {
    var t911 bool = value__214 < 0
    if t911 {
        var t912 uint64 = uint64(int64(value__214))
        var t913 uint64 = 0 - t912
        var t914 string = decimal_string(t913)
        var t915 string = "-" + t914
        return t915
    } else {
        var t916 uint64 = uint64(int64(value__214))
        var t917 string = decimal_string(t916)
        return t917
    }
}

func decimal_string(value__208 uint64) string {
    var t940 bool = value__208 == 0
    if t940 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop933:
        for {
            var t934 bool = remaining__210 > 0
            if t934 {
                var t935_rhs uint64 = 10
                var t935 uint64 = remaining__210 % t935_rhs
                var t936 uint8 = uint8(uint64(t935))
                var t937 uint8 = t936 + 48
                vec_push__Vec_5uint8(reversed__209, t937)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t938 uint64 = compound_old353 / compound_value354
                remaining__210 = t938
                continue
            } else {
                break Loop_loop933
            }
        }
        var t922 int
        var inline1019 int = vec_len__Vec_5uint8(reversed__209)
        t922 = inline1019
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t922)
        var offset__212 int = 0
        Loop_loop924:
        for {
            var t925 int
            var inline1017 int = vec_len__Vec_5uint8(reversed__209)
            t925 = inline1017
            var t926 bool = offset__212 < t925
            if t926 {
                var t927 int
                var inline1015 int = vec_len__Vec_5uint8(reversed__209)
                t927 = inline1015
                var t928 int = t927 - offset__212
                var t929 int = t928 - 1
                var t930 uint8 = vec_get__Vec_5uint8(reversed__209, t929)
                vec_push__Vec_5uint8(bytes__211, t930)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t931 int = compound_old358 + compound_value359
                offset__212 = t931
                continue
            } else {
                break Loop_loop924
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
