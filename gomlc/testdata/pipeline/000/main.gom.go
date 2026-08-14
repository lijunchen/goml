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
    var x188 int32 = self__1.x
    var x189 int32 = self__1.y
    var x190 Color = self__1.color
    var t204 string = "Point { " + "x: "
    var t205 string
    var inline260 string = _goml_runtime_core_int32_to_string(x188)
    t205 = inline260
    var t206 string = t204 + t205
    var t207 string = t206 + ", "
    var t208 string = t207 + "y: "
    var t209 string
    var inline258 string = _goml_runtime_core_int32_to_string(x189)
    t209 = inline258
    var t210 string = t208 + t209
    var t211 string = t210 + ", "
    var t212 string = t211 + "color: "
    var t213 string
    switch x190 {
    case Red:
        t213 = "Color::Red"
    case Green:
        t213 = "Color::Green"
    case Blue:
        t213 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t214 string = t212 + t213
    var t215 string = t214 + " }"
    return t215
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x192 Point = self__8.from
    var x193 Point = self__8.to
    var x194 Color = self__8.color
    var t221 string = "Line { " + "from: "
    var t222 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x192)
    var t223 string = t221 + t222
    var t224 string = t223 + ", "
    var t225 string = t224 + "to: "
    var t226 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x193)
    var t227 string = t225 + t226
    var t228 string = t227 + ", "
    var t229 string = t228 + "color: "
    var t230 string
    switch x194 {
    case Red:
        t230 = "Color::Red"
    case Green:
        t230 = "Color::Green"
    case Blue:
        t230 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t231 string = t229 + t230
    var t232 string = t231 + " }"
    return t232
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x195 Line = self__15.(Cons)._0
        var x196 LineList = self__15.(Cons)._1
        var t240 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x195)
        var t241 string = "LineList::Cons(" + t240
        var t242 string = t241 + ", "
        var t243 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x196)
        var t244 string = t242 + t243
        var t245 string = t244 + ")"
        return t245
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline272 int32 = 10
    var inline273 int32 = 20
    var inline274 Point = Point{
        x: inline272,
        y: inline273,
        color: Red,
    }
    from__18 = inline274
    var to__19 Point
    var inline268 int32 = 30
    var inline269 int32 = 40
    var inline270 Point = Point{
        x: inline268,
        y: inline269,
        color: Green,
    }
    to__19 = inline270
    var line__20 Line
    var inline266 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline266
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t247 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline263)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
