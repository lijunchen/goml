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

type Line struct {
    from Point
    to Point
    color Color
}

type Ordering int32

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func line_to_string(l__4 Line) string {
    var x800 Point = l__4.from
    var x801 Point = l__4.to
    var x802 Color = l__4.color
    var t821 string
    var inline914 int32 = x800.x
    var inline915 int32 = x800.y
    var inline918 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline914)
    var inline919 string = "Point { x: " + inline918
    var inline920 string = inline919 + ", y: "
    var inline921 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline915)
    var inline922 string = inline920 + inline921
    var inline923 string = inline922 + " }"
    t821 = inline923
    var t822 string = "Line { from: " + t821
    var t823 string = t822 + ", to: "
    var t824 string
    var inline902 int32 = x801.x
    var inline903 int32 = x801.y
    var inline906 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline902)
    var inline907 string = "Point { x: " + inline906
    var inline908 string = inline907 + ", y: "
    var inline909 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline903)
    var inline910 string = inline908 + inline909
    var inline911 string = inline910 + " }"
    t824 = inline911
    var t825 string = t823 + t824
    var t826 string = t825 + ", color: "
    var t827 string
    switch x802 {
    case Red:
        t827 = "Red"
    case Green:
        t827 = "Green"
    case Blue:
        t827 = "Blue"
    default:
        panic("non-exhaustive match")
    }
    var t828 string = t826 + t827
    var t829 string = t828 + " }"
    return t829
}

func main0() struct{} {
    var p0__10 Point = Point{
        x: 0,
        y: 0,
    }
    var t841 string
    var inline931 int32 = 0
    var inline932 int32 = 0
    switch inline931 {
    case 0:
        switch inline932 {
        case 0:
            t841 = "origin"
        case 1:
            t841 = "up"
        default:
            var inline934 bool = 0 < inline932
            switch inline934 {
            case true:
                t841 = "above"
            case false:
                t841 = "below"
            default:
                panic("non-exhaustive match")
            }
        }
    case 1:
        switch inline932 {
        case 0:
            t841 = "right"
        default:
            t841 = "unknown"
        }
    default:
        t841 = "unknown"
    }
    var inline928 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t841)
    _goml_runtime_core_string_println(inline928)
    var p1__11 Point = Point{
        x: 10,
        y: 10,
    }
    var line__12 Line = Line{
        from: p0__10,
        to: p1__11,
        color: Red,
    }
    var t842 string = line_to_string(line__12)
    var inline925 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t842)
    _goml_runtime_core_string_println(inline925)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline936 int64 = int64(int32(self__286))
    var inline937 string = signed_decimal_string(inline936)
    return inline937
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t860 bool = value__214 < 0
    if t860 {
        var t861 uint64 = uint64(int64(value__214))
        var t862 uint64 = 0 - t861
        var t863 string = decimal_string(t862)
        var t864 string = "-" + t863
        return t864
    } else {
        var t865 uint64 = uint64(int64(value__214))
        var t866 string = decimal_string(t865)
        return t866
    }
}

func decimal_string(value__208 uint64) string {
    var t889 bool = value__208 == 0
    if t889 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop882:
        for {
            var t883 bool = remaining__210 > 0
            if t883 {
                var t884_rhs uint64 = 10
                var t884 uint64 = remaining__210 % t884_rhs
                var t885 uint8 = uint8(uint64(t884))
                var t886 uint8 = t885 + 48
                vec_push__Vec_5uint8(reversed__209, t886)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t887 uint64 = compound_old353 / compound_value354
                remaining__210 = t887
                continue
            } else {
                break Loop_loop882
            }
        }
        var t871 int
        var inline956 int = vec_len__Vec_5uint8(reversed__209)
        t871 = inline956
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t871)
        var offset__212 int = 0
        Loop_loop873:
        for {
            var t874 int
            var inline954 int = vec_len__Vec_5uint8(reversed__209)
            t874 = inline954
            var t875 bool = offset__212 < t874
            if t875 {
                var t876 int
                var inline952 int = vec_len__Vec_5uint8(reversed__209)
                t876 = inline952
                var t877 int = t876 - offset__212
                var t878 int = t877 - 1
                var t879 uint8 = vec_get__Vec_5uint8(reversed__209, t878)
                vec_push__Vec_5uint8(bytes__211, t879)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t880 int = compound_old358 + compound_value359
                offset__212 = t880
                continue
            } else {
                break Loop_loop873
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
