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
    color Color
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

type LineList interface {
    isLineList()
}

type Nil struct {}

func (_ Nil) isLineList() {}

type Cons struct {
    _0 Line
    _1 LineList
}

func (_ Cons) isLineList() {}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var x797 int32 = self__1.x
    var x798 int32 = self__1.y
    var x799 Color = self__1.color
    var t813 string = "Point { " + "x: "
    var t814 string
    var inline912 string = __goml_builtin_int32_to_string(x797)
    t814 = inline912
    var t815 string = t813 + t814
    var t816 string = t815 + ", "
    var t817 string = t816 + "y: "
    var t818 string
    var inline910 string = __goml_builtin_int32_to_string(x798)
    t818 = inline910
    var t819 string = t817 + t818
    var t820 string = t819 + ", "
    var t821 string = t820 + "color: "
    var t822 string
    switch x799 {
    case Red:
        t822 = "Color::Red"
    case Green:
        t822 = "Color::Green"
    case Blue:
        t822 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t823 string = t821 + t822
    var t824 string = t823 + " }"
    return t824
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x801 Point = self__8.from
    var x802 Point = self__8.to
    var x803 Color = self__8.color
    var t830 string = "Line { " + "from: "
    var t831 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x801)
    var t832 string = t830 + t831
    var t833 string = t832 + ", "
    var t834 string = t833 + "to: "
    var t835 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x802)
    var t836 string = t834 + t835
    var t837 string = t836 + ", "
    var t838 string = t837 + "color: "
    var t839 string
    switch x803 {
    case Red:
        t839 = "Color::Red"
    case Green:
        t839 = "Color::Green"
    case Blue:
        t839 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t840 string = t838 + t839
    var t841 string = t840 + " }"
    return t841
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x804 Line = self__15.(Cons)._0
        var x805 LineList = self__15.(Cons)._1
        var t849 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x804)
        var t850 string = "LineList::Cons(" + t849
        var t851 string = t850 + ", "
        var t852 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x805)
        var t853 string = t851 + t852
        var t854 string = t853 + ")"
        return t854
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline924 int32 = 10
    var inline925 int32 = 20
    var inline926 Point = Point{
        x: inline924,
        y: inline925,
        color: Red,
    }
    from__18 = inline926
    var to__19 Point
    var inline920 int32 = 30
    var inline921 int32 = 40
    var inline922 Point = Point{
        x: inline920,
        y: inline921,
        color: Green,
    }
    to__19 = inline922
    var line__20 Line
    var inline918 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline918
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t856 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline915 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t856)
    _goml_runtime_core_string_println(inline915)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t865 int64 = int64(int32(value__225))
    var inline932 bool = t865 < 0
    if inline932 {
        var inline933 uint64 = uint64(int64(t865))
        var inline934 uint64 = 0 - inline933
        var inline935 string = decimal_string(inline934)
        var inline936 string = "-" + inline935
        return inline936
    } else {
        var inline937 uint64 = uint64(int64(t865))
        var inline938 string = decimal_string(inline937)
        return inline938
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t902 bool = value__208 == 0
    if t902 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop895:
        for {
            var t896 bool = remaining__210 > 0
            if t896 {
                var t897_rhs uint64 = 10
                var t897 uint64 = remaining__210 % t897_rhs
                var t898 uint8 = uint8(uint64(t897))
                var t899 uint8 = t898 + 48
                vec_push__Vec_5uint8(reversed__209, t899)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t900 uint64 = compound_old353 / compound_value354
                remaining__210 = t900
                continue
            } else {
                break Loop_loop895
            }
        }
        var t884 int
        var inline948 int = vec_len__Vec_5uint8(reversed__209)
        t884 = inline948
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t884)
        var offset__212 int = 0
        Loop_loop886:
        for {
            var t887 int
            var inline946 int = vec_len__Vec_5uint8(reversed__209)
            t887 = inline946
            var t888 bool = offset__212 < t887
            if t888 {
                var t889 int
                var inline944 int = vec_len__Vec_5uint8(reversed__209)
                t889 = inline944
                var t890 int = t889 - offset__212
                var t891 int = t890 - 1
                var t892 uint8 = vec_get__Vec_5uint8(reversed__209, t891)
                vec_push__Vec_5uint8(bytes__211, t892)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t893 int = compound_old358 + compound_value359
                offset__212 = t893
                continue
            } else {
                break Loop_loop886
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
