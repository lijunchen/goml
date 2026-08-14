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
    var x183 int32 = self__1.x
    var x184 int32 = self__1.y
    var x185 Color = self__1.color
    var t199 string = "Point { " + "x: "
    var t200 string
    var inline255 string = _goml_runtime_core_int32_to_string(x183)
    t200 = inline255
    var t201 string = t199 + t200
    var t202 string = t201 + ", "
    var t203 string = t202 + "y: "
    var t204 string
    var inline253 string = _goml_runtime_core_int32_to_string(x184)
    t204 = inline253
    var t205 string = t203 + t204
    var t206 string = t205 + ", "
    var t207 string = t206 + "color: "
    var t208 string
    switch x185 {
    case Red:
        t208 = "Color::Red"
    case Green:
        t208 = "Color::Green"
    case Blue:
        t208 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t209 string = t207 + t208
    var t210 string = t209 + " }"
    return t210
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x187 Point = self__8.from
    var x188 Point = self__8.to
    var x189 Color = self__8.color
    var t216 string = "Line { " + "from: "
    var t217 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x187)
    var t218 string = t216 + t217
    var t219 string = t218 + ", "
    var t220 string = t219 + "to: "
    var t221 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x188)
    var t222 string = t220 + t221
    var t223 string = t222 + ", "
    var t224 string = t223 + "color: "
    var t225 string
    switch x189 {
    case Red:
        t225 = "Color::Red"
    case Green:
        t225 = "Color::Green"
    case Blue:
        t225 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t226 string = t224 + t225
    var t227 string = t226 + " }"
    return t227
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x190 Line = self__15.(Cons)._0
        var x191 LineList = self__15.(Cons)._1
        var t235 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x190)
        var t236 string = "LineList::Cons(" + t235
        var t237 string = t236 + ", "
        var t238 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x191)
        var t239 string = t237 + t238
        var t240 string = t239 + ")"
        return t240
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline267 int32 = 10
    var inline268 int32 = 20
    var inline269 Point = Point{
        x: inline267,
        y: inline268,
        color: Red,
    }
    from__18 = inline269
    var to__19 Point
    var inline263 int32 = 30
    var inline264 int32 = 40
    var inline265 Point = Point{
        x: inline263,
        y: inline264,
        color: Green,
    }
    to__19 = inline265
    var line__20 Line
    var inline261 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline261
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t242 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t242)
    _goml_runtime_core_string_println(inline258)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
