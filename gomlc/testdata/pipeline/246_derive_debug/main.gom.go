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

type Point struct {
    x int32
    label string
}

type Ordering int32

type State__i32 struct {
    _tag int32
    _v1_0 int32
    _v2_0 int32
}

type State__Point interface {
    isState__Point()
}

type State__Point_Idle struct {}

func (_ State__Point_Idle) isState__Point() {}

type State__Point_Value struct {
    _0 Point
}

func (_ State__Point_Value) isState__Point() {}

type State__Point_Named struct {
    _0 Point
}

func (_ State__Point_Named) isState__Point() {}

type State__isize struct {
    _tag int32
    _v1_0 int
    _v2_0 int
}

type dyn__Debug_vtable struct {
    debug func(any) string
}

type dyn__Debug struct {
    data any
    vtable *dyn__Debug_vtable
}

func dyn__Debug__wrap__int__debug(self any) string {
    return _goml_m_trait__impl_i_Debug_i_isize_i_debug(self.(int))
}

func dyn__Debug__vtable__int() *dyn__Debug_vtable {
    return &dyn__Debug_vtable{
        debug: dyn__Debug__wrap__int__debug,
    }
}

func _goml_m_trait__impl_i_Debug_i_Point_i_debug(self__0 Point) string {
    var x797 int32 = self__0.x
    var x798 string = self__0.label
    var t807 string = "Point { " + "x: "
    var t808 string
    var inline929 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x797)
    t808 = inline929
    var t809 string = t807 + t808
    var t810 string = t809 + ", "
    var t811 string = t810 + "label: "
    var t812 string
    var inline927 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x798)
    t812 = inline927
    var t813 string = t811 + t812
    var t814 string = t813 + " }"
    return t814
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 3,
        label: "east",
    }
    var idle__8 State__i32 = State__i32{
        _tag: 0,
    }
    var t819 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline969 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t819)
    _goml_runtime_core_string_println(inline969)
    var t820 string = _goml_m_trait__impl_i_Debug_i_State____i32_i_debug(idle__8)
    var inline966 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline966)
    var t821 string
    var inline956 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline957 string = "State::Value(" + inline956
    var inline958 string = inline957 + ")"
    t821 = inline958
    var inline951 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
    _goml_runtime_core_string_println(inline951)
    var t822 string
    var inline944 int = 7
    var inline946 string = "State::Named { " + "value: "
    var inline947 string = _goml_m_trait__impl_i_Debug_i_isize_i_debug(inline944)
    var inline948 string = inline946 + inline947
    var inline949 string = inline948 + " }"
    t822 = inline949
    var inline936 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
    _goml_runtime_core_string_println(inline936)
    var t823 dyn__Debug = dyn__Debug{
        data: int(9),
        vtable: dyn__Debug__vtable__int(),
    }
    var t824 string
    var inline934 string = t823.vtable.debug(t823.data)
    t824 = inline934
    var inline931 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
    _goml_runtime_core_string_println(inline931)
    return struct{}{}
}

func _goml_m_trait__impl_i_Debug_i_State____i32_i_debug(self__3 State__i32) string {
    switch self__3._tag {
    case 0:
        return "State::Idle"
    case 1:
        var x799 int32 = self__3._v1_0
        var t839 string
        var inline976 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x799)
        t839 = inline976
        var t840 string = "State::Value(" + t839
        var t841 string = t840 + ")"
        return t841
    case 2:
        var x800 int32 = self__3._v2_0
        var t842 string = "State::Named { " + "value: "
        var t843 string
        var inline978 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x800)
        t843 = inline978
        var t844 string = t842 + t843
        var t845 string = t844 + " }"
        return t845
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_isize_i_debug(self__419 int) string {
    var inline1012 string = __goml_builtin_int_to_string(self__419)
    return inline1012
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1014 int64 = int64(int32(self__407))
    var inline1015 string = signed_decimal_string(inline1014)
    return inline1015
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t885 int64 = int64(int(value__222))
    var inline1028 bool = t885 < 0
    if inline1028 {
        var inline1029 uint64 = uint64(int64(t885))
        var inline1030 uint64 = 0 - inline1029
        var inline1031 string = decimal_string(inline1030)
        var inline1032 string = "-" + inline1031
        return inline1032
    } else {
        var inline1033 uint64 = uint64(int64(t885))
        var inline1034 string = decimal_string(inline1033)
        return inline1034
    }
}

func signed_decimal_string(value__214 int64) string {
    var t891 bool = value__214 < 0
    if t891 {
        var t892 uint64 = uint64(int64(value__214))
        var t893 uint64 = 0 - t892
        var t894 string = decimal_string(t893)
        var t895 string = "-" + t894
        return t895
    } else {
        var t896 uint64 = uint64(int64(value__214))
        var t897 string = decimal_string(t896)
        return t897
    }
}

func decimal_string(value__208 uint64) string {
    var t920 bool = value__208 == 0
    if t920 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop913:
        for {
            var t914 bool = remaining__210 > 0
            if t914 {
                var t915_rhs uint64 = 10
                var t915 uint64 = remaining__210 % t915_rhs
                var t916 uint8 = uint8(uint64(t915))
                var t917 uint8 = t916 + 48
                vec_push__Vec_5uint8(reversed__209, t917)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t918 uint64 = compound_old353 / compound_value354
                remaining__210 = t918
                continue
            } else {
                break Loop_loop913
            }
        }
        var t902 int
        var inline1044 int = vec_len__Vec_5uint8(reversed__209)
        t902 = inline1044
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t902)
        var offset__212 int = 0
        Loop_loop904:
        for {
            var t905 int
            var inline1042 int = vec_len__Vec_5uint8(reversed__209)
            t905 = inline1042
            var t906 bool = offset__212 < t905
            if t906 {
                var t907 int
                var inline1040 int = vec_len__Vec_5uint8(reversed__209)
                t907 = inline1040
                var t908 int = t907 - offset__212
                var t909 int = t908 - 1
                var t910 uint8 = vec_get__Vec_5uint8(reversed__209, t909)
                vec_push__Vec_5uint8(bytes__211, t910)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t911 int = compound_old358 + compound_value359
                offset__212 = t911
                continue
            } else {
                break Loop_loop904
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
