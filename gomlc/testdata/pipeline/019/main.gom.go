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
    y int32
}

type Wrapper__i32 struct {
    value int32
}

type Ordering int32

func make_point() Point {
    var t826 Point = Point{
        x: 0,
        y: 0,
    }
    return t826
}

func flip(point__0 Point) Point {
    var x797 int32 = point__0.x
    var x798 int32 = point__0.y
    var t829 Point = Point{
        x: x798,
        y: x797,
    }
    return t829
}

func x_add_1(p__4 Point) Point {
    var x800 int32 = p__4.x
    var x801 int32 = p__4.y
    var t835 int32 = x800 + 1
    var t836 Point = Point{
        x: t835,
        y: x801,
    }
    return t836
}

func point32_to_string(p__13 Point) string {
    var x809 int32 = p__13.x
    var x810 int32 = p__13.y
    var t843 string
    var inline934 string = __goml_builtin_int32_to_string(x809)
    t843 = inline934
    var t844 string = "Point { x: " + t843
    var t845 string = t844 + ", y: "
    var t846 string
    var inline932 string = __goml_builtin_int32_to_string(x810)
    t846 = inline932
    var t847 string = t845 + t846
    var t848 string = t847 + "}"
    return t848
}

func point32_to_string2(p__16 Point) string {
    var x812 int32 = p__16.x
    var x813 int32 = p__16.y
    var t851 string
    var inline938 string = __goml_builtin_int32_to_string(x812)
    t851 = inline938
    var t852 string = "Point { x: " + t851
    var t853 string = t852 + ", y: "
    var t854 string
    var inline936 string = __goml_builtin_int32_to_string(x813)
    t854 = inline936
    var t855 string = t853 + t854
    var t856 string = t855 + "}"
    return t856
}

func main0() struct{} {
    var start__25 Point = make_point()
    var t874 string = point32_to_string(start__25)
    println__T_string(t874)
    var t875 Point = Point{
        x: 1,
        y: 2,
    }
    var swapped__26 Point = flip(t875)
    var t876 string = point32_to_string2(swapped__26)
    println__T_string(t876)
    var a__29 Point = x_add_1(start__25)
    var t877 string
    var inline982 int32 = a__29.x
    var inline983 int32 = a__29.y
    var inline986 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline982)
    var inline987 string = "Point { x: " + inline986
    var inline988 string = inline987 + ", y: "
    var inline989 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline983)
    var inline990 string = inline988 + inline989
    var inline991 string = inline990 + "}"
    t877 = inline991
    var inline978 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t877)
    _goml_runtime_core_string_println(inline978)
    var t878 Point
    var inline971 int32 = start__25.x
    var inline972 int32 = start__25.y
    var inline975 int32 = inline971 + 1
    var inline976 Point = Point{
        x: inline975,
        y: inline972,
    }
    t878 = inline976
    var a__30 Point
    var inline964 int32 = t878.x
    var inline965 int32 = t878.y
    var inline968 Point = Point{
        x: inline965,
        y: inline964,
    }
    a__30 = inline968
    var t879 string
    var inline952 int32 = a__30.x
    var inline953 int32 = a__30.y
    var inline956 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline952)
    var inline957 string = "Point { x: " + inline956
    var inline958 string = inline957 + ", y: "
    var inline959 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline953)
    var inline960 string = inline958 + inline959
    var inline961 string = inline960 + "}"
    t879 = inline961
    var inline948 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t879)
    _goml_runtime_core_string_println(inline948)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline996 int64 = int64(int32(self__286))
    var inline997 string = signed_decimal_string(inline996)
    return inline997
}

func println__T_string(value__1 string) struct{} {
    var t884 string
    t884 = value__1
    _goml_runtime_core_string_println(t884)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t888 int64 = int64(int32(value__225))
    var inline1000 bool = t888 < 0
    if inline1000 {
        var inline1001 uint64 = uint64(int64(t888))
        var inline1002 uint64 = 0 - inline1001
        var inline1003 string = decimal_string(inline1002)
        var inline1004 string = "-" + inline1003
        return inline1004
    } else {
        var inline1005 uint64 = uint64(int64(t888))
        var inline1006 string = decimal_string(inline1005)
        return inline1006
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t896 bool = value__214 < 0
    if t896 {
        var t897 uint64 = uint64(int64(value__214))
        var t898 uint64 = 0 - t897
        var t899 string = decimal_string(t898)
        var t900 string = "-" + t899
        return t900
    } else {
        var t901 uint64 = uint64(int64(value__214))
        var t902 string = decimal_string(t901)
        return t902
    }
}

func decimal_string(value__208 uint64) string {
    var t925 bool = value__208 == 0
    if t925 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop918:
        for {
            var t919 bool = remaining__210 > 0
            if t919 {
                var t920_rhs uint64 = 10
                var t920 uint64 = remaining__210 % t920_rhs
                var t921 uint8 = uint8(uint64(t920))
                var t922 uint8 = t921 + 48
                vec_push__Vec_5uint8(reversed__209, t922)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t923 uint64 = compound_old353 / compound_value354
                remaining__210 = t923
                continue
            } else {
                break Loop_loop918
            }
        }
        var t907 int
        var inline1016 int = vec_len__Vec_5uint8(reversed__209)
        t907 = inline1016
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t907)
        var offset__212 int = 0
        Loop_loop909:
        for {
            var t910 int
            var inline1014 int = vec_len__Vec_5uint8(reversed__209)
            t910 = inline1014
            var t911 bool = offset__212 < t910
            if t911 {
                var t912 int
                var inline1012 int = vec_len__Vec_5uint8(reversed__209)
                t912 = inline1012
                var t913 int = t912 - offset__212
                var t914 int = t913 - 1
                var t915 uint8 = vec_get__Vec_5uint8(reversed__209, t914)
                vec_push__Vec_5uint8(bytes__211, t915)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t916 int = compound_old358 + compound_value359
                offset__212 = t916
                continue
            } else {
                break Loop_loop909
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
