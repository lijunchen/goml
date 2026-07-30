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
}

type Node struct {
    value int32
    next List
}

type Wrapper__int32 struct {
    value int32
}

type closure_env_add_0 struct {
    offset_0 int32
}

type closure_env_id_1 struct {}

type List interface {
    isList()
}

type Cons struct {
    _0 Node
}

func (_ Cons) isList() {}

type Nil struct {}

func (_ Nil) isList() {}

type Shape__int32 interface {
    isShape__int32()
}

type Dot struct {
    _0 Point
}

func (_ Dot) isShape__int32() {}

type Wrapped struct {
    _0 Wrapper__int32
}

func (_ Wrapped) isShape__int32() {}

type Origin struct {}

func (_ Origin) isShape__int32() {}

func _goml_m_inherent_i_Point_i_Point_i_new(x__0 int32, y__1 int32) Point {
    var retv114 Point
    var t115 Point = Point{
        x: x__0,
        y: y__1,
    }
    retv114 = t115
    return retv114
}

func _goml_m_inherent_i_Point_i_Point_i_copy(self__2 Point, other__3 Point) Point {
    var retv117 Point
    var t118 int32 = self__2.x
    var t119 int32 = other__3.x
    var t120 int32 = t118 + t119
    var t121 int32 = self__2.y
    var t122 int32 = other__3.y
    var t123 int32 = t121 + t122
    var t124 Point = Point{
        x: t120,
        y: t123,
    }
    retv117 = t124
    return retv117
}

func _goml_m_inherent_i_Point_i_Point_i_origin() Point {
    var retv126 Point
    var t127 Point = Point{
        x: 0,
        y: 0,
    }
    retv126 = t127
    return retv126
}

func shape_value(value__7 Shape__int32) int32 {
    var retv129 int32
    var jp131 int32
    switch value__7.(type) {
    case Dot:
        var x108 Point = value__7.(Dot)._0
        var point__8 Point = x108
        var t132 int32 = point__8.x
        var t133 int32 = point__8.y
        var t134 int32 = t132 + t133
        jp131 = t134
    case Wrapped:
        var x109 Wrapper__int32 = value__7.(Wrapped)._0
        var wrapper__9 Wrapper__int32 = x109
        var t135 int32 = wrapper__9.value
        jp131 = t135
    case Origin:
        jp131 = 0
    default:
        panic("non-exhaustive match")
    }
    retv129 = jp131
    return retv129
}

func list_value(value__10 List) int32 {
    var retv137 int32
    var jp139 int32
    switch value__10.(type) {
    case Cons:
        var x110 Node = value__10.(Cons)._0
        var node__11 Node = x110
        var t140 int32 = node__11.value
        var t141 List = node__11.next
        var t142 int32 = list_value(t141)
        var t143 int32 = t140 + t142
        jp139 = t143
    case Nil:
        jp139 = 0
    default:
        panic("non-exhaustive match")
    }
    retv137 = jp139
    return retv137
}

func main0() struct{} {
    var offset__12 int32 = 1
    var add__14 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var t145 int32 = _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(add__14, 1)
    var point__15 Point = _goml_m_inherent_i_Point_i_Point_i_new(t145, 3)
    var t146 Point = _goml_m_inherent_i_Point_i_Point_i_origin()
    var combined__16 Point = _goml_m_inherent_i_Point_i_Point_i_copy(point__15, t146)
    var t147 int32 = wrap__T_int32(4)
    var t148 Wrapper__int32 = Wrapper__int32{
        value: t147,
    }
    var wrapped__17 Shape__int32 = Wrapped{
        _0: t148,
    }
    var t149 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t149,
    }
    var t150 int32 = combined__16.x
    var t151 int32 = combined__16.y
    var t152 int32 = t150 + t151
    var t153 int32 = shape_value(wrapped__17)
    var t154 int32 = t152 + t153
    var t155 int32 = list_value(list__18)
    var t156 int32 = t154 + t155
    var t157 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t156)
    println__T_string(t157)
    return struct{}{}
}

func wrap__T_int32(value__4 int32) int32 {
    var retv160 int32
    var id__6 closure_env_id_1 = closure_env_id_1{}
    var t161 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(id__6, value__4)
    retv160 = t161
    return retv160
}

func println__T_string(value__1 string) struct{} {
    var t163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t163)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv166 string
    var t167 string = _goml_runtime_core_int32_to_string(self__6)
    retv166 = t167
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv169 string
    retv169 = self__38
    return retv169
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env111 closure_env_add_0, value__13 int32) int32 {
    var retv171 int32
    var offset__12 int32 = env111.offset_0
    var t172 int32 = value__13 + offset__12
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env112 closure_env_id_1, item__5 int32) int32 {
    var retv174 int32
    retv174 = item__5
    return retv174
}

func main() {
    main0()
}
