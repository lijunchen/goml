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
    var x137 int32 = self__1.x
    var x138 int32 = self__1.y
    var x139 Color = self__1.color
    var t153 string = "Point { " + "x: "
    var t154 string
    var inline209 string = _goml_runtime_core_int32_to_string(x137)
    t154 = inline209
    var t155 string = t153 + t154
    var t156 string = t155 + ", "
    var t157 string = t156 + "y: "
    var t158 string
    var inline207 string = _goml_runtime_core_int32_to_string(x138)
    t158 = inline207
    var t159 string = t157 + t158
    var t160 string = t159 + ", "
    var t161 string = t160 + "color: "
    var t162 string
    switch x139 {
    case Red:
        t162 = "Color::Red"
    case Green:
        t162 = "Color::Green"
    case Blue:
        t162 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t163 string = t161 + t162
    var t164 string = t163 + " }"
    return t164
}

func _goml_m_trait__impl_i_ToString_i_Line_i_to__string(self__8 Line) string {
    var x141 Point = self__8.from
    var x142 Point = self__8.to
    var x143 Color = self__8.color
    var t170 string = "Line { " + "from: "
    var t171 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x141)
    var t172 string = t170 + t171
    var t173 string = t172 + ", "
    var t174 string = t173 + "to: "
    var t175 string = _goml_m_trait__impl_i_ToString_i_Point_i_to__string(x142)
    var t176 string = t174 + t175
    var t177 string = t176 + ", "
    var t178 string = t177 + "color: "
    var t179 string
    switch x143 {
    case Red:
        t179 = "Color::Red"
    case Green:
        t179 = "Color::Green"
    case Blue:
        t179 = "Color::Blue"
    default:
        panic("non-exhaustive match")
    }
    var t180 string = t178 + t179
    var t181 string = t180 + " }"
    return t181
}

func _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(self__15 LineList) string {
    switch self__15.(type) {
    case Nil:
        return "LineList::Nil"
    case Cons:
        var x144 Line = self__15.(Cons)._0
        var x145 LineList = self__15.(Cons)._1
        var t189 string = _goml_m_trait__impl_i_ToString_i_Line_i_to__string(x144)
        var t190 string = "LineList::Cons(" + t189
        var t191 string = t190 + ", "
        var t192 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(x145)
        var t193 string = t191 + t192
        var t194 string = t193 + ")"
        return t194
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var from__18 Point
    var inline221 int32 = 10
    var inline222 int32 = 20
    var inline223 Point = Point{
        x: inline221,
        y: inline222,
        color: Red,
    }
    from__18 = inline223
    var to__19 Point
    var inline217 int32 = 30
    var inline218 int32 = 40
    var inline219 Point = Point{
        x: inline217,
        y: inline218,
        color: Green,
    }
    to__19 = inline219
    var line__20 Line
    var inline215 Line = Line{
        from: from__18,
        to: to__19,
        color: Blue,
    }
    line__20 = inline215
    var lines__21 LineList = Cons{
        _0: line__20,
        _1: Nil{},
    }
    var t196 string = _goml_m_trait__impl_i_ToString_i_LineList_i_to__string(lines__21)
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
