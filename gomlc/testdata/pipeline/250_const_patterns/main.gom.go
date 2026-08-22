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

func _goml_intrinsic_missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
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

func missing__string(s string) string {
    _goml_intrinsic_missing(s)
    var ret string
    return ret
}

type Tuple2_3int_5uint8 struct {
    _0 int
    _1 uint8
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

const (
    RATIO float64 = 1.5
    COMPUTED_ENABLED bool = true
    CLASSIFIED_AT_COMPILE_TIME int = 1
)

func classify(value__1 int) string {
    switch value__1 {
    case 42:
        return "known"
    case 7:
        return "known"
    default:
        return "other"
    }
}

func classify_bool(value__2 bool) string {
    switch value__2 {
    case true:
        return "enabled"
    case false:
        return "disabled"
    default:
        panic("non-exhaustive match")
    }
}

func classify_computed_bool(value__3 bool) string {
    var t851 bool = value__3 == COMPUTED_ENABLED
    if t851 {
        return "enabled"
    } else {
        var t854 bool = value__3 == false
        if t854 {
            return "disabled"
        } else {
            var t855 string = missing__string("")
            return t855
        }
    }
}

func classify_comptime_guard(value__4 bool) string {
    var t860 bool = value__4 == true
    var jp859 string
    if t860 {
        jp859 = "enabled"
    } else {
        var t869 bool = value__4 == false
        if t869 {
            jp859 = "disabled"
        } else {
            var t870 string = missing__string("")
            jp859 = t870
        }
    }
    return jp859
}

func classify_pair(value__6 Tuple2_3int_5uint8) bool {
    var x799 int = value__6._0
    var x800 uint8 = value__6._1
    switch x800 {
    case 65:
        switch x799 {
        case 42:
            return true
        default:
            return false
        }
    default:
        return false
    }
}

func classify_string(value__7 string) bool {
    switch value__7 {
    case "hello":
        return true
    default:
        return false
    }
}

func classify_float(value__8 float64) bool {
    var t885 bool = value__8 == RATIO
    if t885 {
        return true
    } else {
        return false
    }
}

func for_binding() int {
    var total__14 *ref_int_x
    var inline1013 int = 0
    var inline1014 *ref_int_x = ref__Ref_3int(inline1013)
    total__14 = inline1014
    var for_source804 [2]int = [2]int{1, 2}
    var for_limit805 int = 2
    var for_index806 int = 0
    Loop_loop902:
    for {
        var t903 bool = for_index806 < for_limit805
        if t903 {
            var for_item807 int = array_get__Array_2_3int(for_source804, for_index806)
            var t904 int = for_index806 + 1
            for_index806 = t904
            var t905 int
            var inline1009 int = ref_get__Ref_3int(total__14)
            t905 = inline1009
            var t906 int = t905 + for_item807
            ref_set__Ref_3int(total__14, t906)
            continue
        } else {
            break Loop_loop902
        }
    }
    var inline1011 int = ref_get__Ref_3int(total__14)
    return inline1011
}

func main0() struct{} {
    var t909 string = classify(42)
    println__T_string(t909)
    var t910 string = classify(7)
    println__T_string(t910)
    var t911 string = classify(0)
    println__T_string(t911)
    var t912 string = classify_bool(true)
    println__T_string(t912)
    var t913 string = classify_bool(false)
    println__T_string(t913)
    var t914 string = classify_computed_bool(true)
    println__T_string(t914)
    var t915 string = classify_computed_bool(false)
    println__T_string(t915)
    var t916 string = classify_comptime_guard(true)
    println__T_string(t916)
    var t917 string = classify_comptime_guard(false)
    println__T_string(t917)
    var t918 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 65,
    }
    var t919 bool = classify_pair(t918)
    println__T_bool(t919)
    var t920 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 66,
    }
    var t921 bool = classify_pair(t920)
    println__T_bool(t921)
    var t922 bool = classify_string("hello")
    println__T_bool(t922)
    var t923 bool = classify_float(1.5)
    var inline1056 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t923)
    _goml_runtime_core_string_println(inline1056)
    var t924 int
    var inline1054 int = 9
    t924 = inline1054
    var inline1051 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t924)
    _goml_runtime_core_string_println(inline1051)
    var t925 int
    var inline1047 int = 11
    t925 = inline1047
    var inline1044 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t925)
    _goml_runtime_core_string_println(inline1044)
    var t926 bool
    var inline1042 int = 42
    switch inline1042 {
    case 42:
        t926 = true
    default:
        t926 = false
    }
    var inline1039 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t926)
    _goml_runtime_core_string_println(inline1039)
    var t927 bool
    var inline1037 int = 41
    switch inline1037 {
    case 42:
        t927 = true
    default:
        t927 = false
    }
    var inline1034 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t927)
    _goml_runtime_core_string_println(inline1034)
    var t928 bool
    var inline1031 int = 42
    switch inline1031 {
    case 42:
        t928 = true
    default:
        t928 = false
    }
    var inline1028 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t928)
    _goml_runtime_core_string_println(inline1028)
    var t929 bool
    var inline1025 int = 41
    switch inline1025 {
    case 42:
        t929 = true
    default:
        t929 = false
    }
    var inline1022 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t929)
    _goml_runtime_core_string_println(inline1022)
    var t930 int = for_binding()
    var inline1019 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t930)
    _goml_runtime_core_string_println(inline1019)
    var inline1016 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(CLASSIFIED_AT_COMPILE_TIME)
    _goml_runtime_core_string_println(inline1016)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t941 string
    t941 = value__1
    _goml_runtime_core_string_println(t941)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t944 string
    var inline1060 string = _goml_runtime_core_bool_to_string(value__1)
    t944 = inline1060
    _goml_runtime_core_string_println(t944)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t953 string = _goml_runtime_core_bool_to_string(self__401)
    return t953
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1064 int64 = int64(int(self__404))
    var inline1065 string = signed_decimal_string(inline1064)
    return inline1065
}

func signed_decimal_string(value__214 int64) string {
    var t965 bool = value__214 < 0
    if t965 {
        var t966 uint64 = uint64(int64(value__214))
        var t967 uint64 = 0 - t966
        var t968 string = decimal_string(t967)
        var t969 string = "-" + t968
        return t969
    } else {
        var t970 uint64 = uint64(int64(value__214))
        var t971 string = decimal_string(t970)
        return t971
    }
}

func decimal_string(value__208 uint64) string {
    var t994 bool = value__208 == 0
    if t994 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop987:
        for {
            var t988 bool = remaining__210 > 0
            if t988 {
                var t989_rhs uint64 = 10
                var t989 uint64 = remaining__210 % t989_rhs
                var t990 uint8 = uint8(uint64(t989))
                var t991 uint8 = t990 + 48
                vec_push__Vec_5uint8(reversed__209, t991)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t992 uint64 = compound_old353 / compound_value354
                remaining__210 = t992
                continue
            } else {
                break Loop_loop987
            }
        }
        var t976 int
        var inline1083 int = vec_len__Vec_5uint8(reversed__209)
        t976 = inline1083
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t976)
        var offset__212 int = 0
        Loop_loop978:
        for {
            var t979 int
            var inline1081 int = vec_len__Vec_5uint8(reversed__209)
            t979 = inline1081
            var t980 bool = offset__212 < t979
            if t980 {
                var t981 int
                var inline1079 int = vec_len__Vec_5uint8(reversed__209)
                t981 = inline1079
                var t982 int = t981 - offset__212
                var t983 int = t982 - 1
                var t984 uint8 = vec_get__Vec_5uint8(reversed__209, t983)
                vec_push__Vec_5uint8(bytes__211, t984)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t985 int = compound_old358 + compound_value359
                offset__212 = t985
                continue
            } else {
                break Loop_loop978
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
