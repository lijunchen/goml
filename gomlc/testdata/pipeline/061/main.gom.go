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
    var retv164 string
    var jp166 string
    switch self__0 {
    case Red:
        jp166 = "Color::Red"
    case Green:
        jp166 = "Color::Green"
    case Blue:
        jp166 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    retv164 = jp166
    return retv164
}

func _goml_m_trait__impl_i_ToString_i_Point_i_to__string(self__1 Point) string {
    var retv168 string
    var mtmp152 Point = self__1
    var x153 int32 = mtmp152.x
    var x154 int32 = mtmp152.y
    var x155 Color = mtmp152.color
    var color__4 Color = x155
    var y__3 int32 = x154
    var x__2 int32 = x153
    var t169 string = "Point { " + "x: "
    var t170 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__2)
    var t171 string = t169 + t170
    var t172 string = t171 + ", "
    var t173 string = t172 + "y: "
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__3)
    var t175 string = t173 + t174
    var t176 string = t175 + ", "
    var t177 string = t176 + "color: "
    var t178 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__4)
    var t179 string = t177 + t178
    var t180 string = t179 + " }"
    retv168 = t180
    return retv168
}

func _goml_m_inherent_i_Point_i_Point_i_new(x__5 int32, y__6 int32, color__7 Color) Point {
    var retv182 Point
    var t183 Point = Point{
        x: x__5,
        y: y__6,
        color: color__7,
    }
    retv182 = t183
    return retv182
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var retv185 string
    var mtmp156 Line = self__8
    var x157 Point = mtmp156.from
    var x158 Point = mtmp156.to
    var x159 Color = mtmp156.color
    var color__11 Color = x159
    var to__10 Point = x158
    var from__9 Point = x157
    var t186 string = "Line { " + "from: "
    var t187 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(from__9)
    var t188 string = t186 + t187
    var t189 string = t188 + ", "
    var t190 string = t189 + "to: "
    var t191 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(to__10)
    var t192 string = t190 + t191
    var t193 string = t192 + ", "
    var t194 string = t193 + "color: "
    var t195 string = _goml_m_trait__impl_i_ToString_i_Color_i_to__string(color__11)
    var t196 string = t194 + t195
    var t197 string = t196 + " }"
    retv185 = t197
    return retv185
}

func _goml_m_inherent_i_Line_i_Line_i_new(from__12 Point, to__13 Point, color__14 Color) Line {
    var retv199 Line
    var t200 Line = Line{
        from: from__12,
        to: to__13,
        color: color__14,
    }
    retv199 = t200
    return retv199
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    var retv202 string
    var jp204 string
    switch self__15.(type) {
    case Nil:
        jp204 = "LineList::Nil"
    case Cons:
        var x160 Line = self__15.(Cons)._0
        var x161 LineList = self__15.(Cons)._1
        var __field1__17 LineList = x161
        var __field0__16 Line = x160
        var t205 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(__field0__16)
        var t206 string = "LineList::Cons(" + t205
        var t207 string = t206 + ", "
        var t208 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(__field1__17)
        var t209 string = t207 + t208
        var t210 string = t209 + ")"
        jp204 = t210
    default:
        panic("non-exhaustive match")
    }
    retv202 = jp204
    return retv202
}

func main0() struct{} {
    var from__18 Point = _goml_m_inherent_i_Point_i_Point_i_new(10, 20, Red)
    var to__19 Point = _goml_m_inherent_i_Point_i_Point_i_new(30, 40, Green)
    var line__20 Line = _goml_m_inherent_i_Line_i_Line_i_new(from__18, to__19, Blue)
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t212 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    println__T_string(t212)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv214 string
    var t215 string = _goml_runtime_core_int32_to_string(self__6)
    retv214 = t215
    return retv214
}

func println__T_string(value__1 string) struct{} {
    var t217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv220 string
    retv220 = self__38
    return retv220
}

func main() {
    main0()
}
