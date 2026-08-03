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
    var x178 int32 = self__1.x
    var x179 int32 = self__1.y
    var x180 Color = self__1.color
    var t194 string = "Point { " + "x: "
    var t195 string
    var inline250 string = _goml_runtime_core_int32_to_string(x178)
    t195 = inline250
    var t196 string = t194 + t195
    var t197 string = t196 + ", "
    var t198 string = t197 + "y: "
    var t199 string
    var inline248 string = _goml_runtime_core_int32_to_string(x179)
    t199 = inline248
    var t200 string = t198 + t199
    var t201 string = t200 + ", "
    var t202 string = t201 + "color: "
    var t203 string
    switch x180 {
    case Red:
        t203 = "Color::Red"
    case Green:
        t203 = "Color::Green"
    case Blue:
        t203 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t204 string = t202 + t203
    var t205 string = t204 + " }"
    return t205
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x182 Point = self__8.from
    var x183 Point = self__8.to
    var x184 Color = self__8.color
    var t211 string = "Line { " + "from: "
    var t212 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x182)
    var t213 string = t211 + t212
    var t214 string = t213 + ", "
    var t215 string = t214 + "to: "
    var t216 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x183)
    var t217 string = t215 + t216
    var t218 string = t217 + ", "
    var t219 string = t218 + "color: "
    var t220 string
    switch x184 {
    case Red:
        t220 = "Color::Red"
    case Green:
        t220 = "Color::Green"
    case Blue:
        t220 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t221 string = t219 + t220
    var t222 string = t221 + " }"
    return t222
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x185 Line = self__15.(Cons)._0
        var x186 LineList = self__15.(Cons)._1
        var t230 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x185)
        var t231 string = "LineList::Cons(" + t230
        var t232 string = t231 + ", "
        var t233 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x186)
        var t234 string = t232 + t233
        var t235 string = t234 + ")"
        return t235
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline262 int32 = 10
    var inline263 int32 = 20
    var inline264 Point = Point{
        x: inline262,
        y: inline263,
        color: Red,
    }
    from__18 = inline264
    var to__19 Point
    var inline258 int32 = 30
    var inline259 int32 = 40
    var inline260 Point = Point{
        x: inline258,
        y: inline259,
        color: Green,
    }
    to__19 = inline260
    var line__20 Line
    var inline256 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline256
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t237 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline253)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
