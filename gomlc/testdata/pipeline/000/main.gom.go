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
    var x173 int32 = self__1.x
    var x174 int32 = self__1.y
    var x175 Color = self__1.color
    var t189 string = "Point { " + "x: "
    var t190 string
    var inline245 string = _goml_runtime_core_int32_to_string(x173)
    t190 = inline245
    var t191 string = t189 + t190
    var t192 string = t191 + ", "
    var t193 string = t192 + "y: "
    var t194 string
    var inline243 string = _goml_runtime_core_int32_to_string(x174)
    t194 = inline243
    var t195 string = t193 + t194
    var t196 string = t195 + ", "
    var t197 string = t196 + "color: "
    var t198 string
    switch x175 {
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

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x177 Point = self__8.from
    var x178 Point = self__8.to
    var x179 Color = self__8.color
    var t206 string = "Line { " + "from: "
    var t207 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x177)
    var t208 string = t206 + t207
    var t209 string = t208 + ", "
    var t210 string = t209 + "to: "
    var t211 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x178)
    var t212 string = t210 + t211
    var t213 string = t212 + ", "
    var t214 string = t213 + "color: "
    var t215 string
    switch x179 {
    case Red:
        t215 = "Color::Red"
    case Green:
        t215 = "Color::Green"
    case Blue:
        t215 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t216 string = t214 + t215
    var t217 string = t216 + " }"
    return t217
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x180 Line = self__15.(Cons)._0
        var x181 LineList = self__15.(Cons)._1
        var t225 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x180)
        var t226 string = "LineList::Cons(" + t225
        var t227 string = t226 + ", "
        var t228 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x181)
        var t229 string = t227 + t228
        var t230 string = t229 + ")"
        return t230
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline257 int32 = 10
    var inline258 int32 = 20
    var inline259 Point = Point{
        x: inline257,
        y: inline258,
        color: Red,
    }
    from__18 = inline259
    var to__19 Point
    var inline253 int32 = 30
    var inline254 int32 = 40
    var inline255 Point = Point{
        x: inline253,
        y: inline254,
        color: Green,
    }
    to__19 = inline255
    var line__20 Line
    var inline251 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline251
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t232 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline248)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
