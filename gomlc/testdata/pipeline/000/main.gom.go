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

func _goml_m_trait__impl_i_ToString_i_Color_i_to__string(self__0 Color) string {
    switch self__0 {
    case Red:
        return "Color::Red"
    case Green:
        return "Color::Green"
    case Blue:
        return "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var x156 int32 = self__1.x
    var x157 int32 = self__1.y
    var x158 Color = self__1.color
    var t172 string = "Point { " + "x: "
    var t173 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x156)
    var t174 string = t172 + t173
    var t175 string = t174 + ", "
    var t176 string = t175 + "y: "
    var t177 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x157)
    var t178 string = t176 + t177
    var t179 string = t178 + ", "
    var t180 string = t179 + "color: "
    var t181 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(x158)
    var t182 string = t180 + t181
    var t183 string = t182 + " }"
    return t183
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var t186 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    return t186
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
    var t198 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(x162)
    var t199 string = t197 + t198
    var t200 string = t199 + " }"
    return t200
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var t203 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    return t203
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
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t215 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t215)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t218 string = _goml_runtime_core_int32_to_string(self__6)
    return t218
}

func println__T_string(value__1 string) struct{} {
    var t220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t220)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
