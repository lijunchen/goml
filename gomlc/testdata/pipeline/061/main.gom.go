package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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
    var x156 int32 = self__1.x
    var x157 int32 = self__1.y
    var x158 Color = self__1.color
    var t172 string = "Point { " + "x: "
    var t173 string
    var inline228 string = _goml_runtime_core_int32_to_string(x156)
    t173 = inline228
    var t174 string = t172 + t173
    var t175 string = t174 + ", "
    var t176 string = t175 + "y: "
    var t177 string
    var inline226 string = _goml_runtime_core_int32_to_string(x157)
    t177 = inline226
    var t178 string = t176 + t177
    var t179 string = t178 + ", "
    var t180 string = t179 + "color: "
    var t181 string
    switch x158 {
    case Red:
        t181 = "Color::Red"
    case Green:
        t181 = "Color::Green"
    case Blue:
        t181 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t182 string = t180 + t181
    var t183 string = t182 + " }"
    return t183
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x160 Point = self__8.from
    var x161 Point = self__8.to
    var x162 Color = self__8.color
    var t189 string = "Line { " + "from: "
    var t190 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x160)
    var t191 string = t189 + t190
    var t192 string = t191 + ", "
    var t193 string = t192 + "to: "
    var t194 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x161)
    var t195 string = t193 + t194
    var t196 string = t195 + ", "
    var t197 string = t196 + "color: "
    var t198 string
    switch x162 {
    case Red:
        t198 = "Color::Red"
    case Green:
        t198 = "Color::Green"
    case Blue:
        t198 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t199 string = t197 + t198
    var t200 string = t199 + " }"
    return t200
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x163 Line = self__15.(Cons)._0
        var x164 LineList = self__15.(Cons)._1
        var t208 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x163)
        var t209 string = "LineList::Cons(" + t208
        var t210 string = t209 + ", "
        var t211 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x164)
        var t212 string = t210 + t211
        var t213 string = t212 + ")"
        return t213
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline240 int32 = 10
    var inline241 int32 = 20
    var inline242 Point = Point{
        x: inline240,
        y: inline241,
        color: Red,
    }
    from__18 = inline242
    var to__19 Point
    var inline236 int32 = 30
    var inline237 int32 = 40
    var inline238 Point = Point{
        x: inline236,
        y: inline237,
        color: Green,
    }
    to__19 = inline238
    var line__20 Line
    var inline234 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline234
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t215 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
