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
    var retv70 Point
    var t71 Point = Point{
        x: x__0,
        y: y__1,
    }
    retv70 = t71
    return retv70
}

func _goml_m_inherent_i_Point_i_Point_i_copy(self__2 Point, other__3 Point) Point {
    var retv73 Point
    var t74 int32 = self__2.x
    var t75 int32 = other__3.x
    var t76 int32 = t74 + t75
    var t77 int32 = self__2.y
    var t78 int32 = other__3.y
    var t79 int32 = t77 + t78
    var t80 Point = Point{
        x: t76,
        y: t79,
    }
    retv73 = t80
    return retv73
}

func _goml_m_inherent_i_Point_i_Point_i_origin() Point {
    var retv82 Point
    var t83 Point = Point{
        x: 0,
        y: 0,
    }
    retv82 = t83
    return retv82
}

func shape_value(value__7 Shape__int32) int32 {
    var retv85 int32
    var jp87 int32
    switch value__7.(type) {
    case Dot:
        var x64 Point = value__7.(Dot)._0
        var point__8 Point = x64
        var t88 int32 = point__8.x
        var t89 int32 = point__8.y
        var t90 int32 = t88 + t89
        jp87 = t90
    case Wrapped:
        var x65 Wrapper__int32 = value__7.(Wrapped)._0
        var wrapper__9 Wrapper__int32 = x65
        var t91 int32 = wrapper__9.value
        jp87 = t91
    case Origin:
        jp87 = 0
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func list_value(value__10 List) int32 {
    var retv93 int32
    var jp95 int32
    switch value__10.(type) {
    case Cons:
        var x66 Node = value__10.(Cons)._0
        var node__11 Node = x66
        var t96 int32 = node__11.value
        var t97 List = node__11.next
        var t98 int32 = list_value(t97)
        var t99 int32 = t96 + t98
        jp95 = t99
    case Nil:
        jp95 = 0
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var offset__12 int32 = 1
    var add__14 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var t101 int32 = _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(add__14, 1)
    var point__15 Point = _goml_m_inherent_i_Point_i_Point_i_new(t101, 3)
    var t102 Point = _goml_m_inherent_i_Point_i_Point_i_origin()
    var combined__16 Point = _goml_m_inherent_i_Point_i_Point_i_copy(point__15, t102)
    var t103 int32 = wrap__T_int32(4)
    var t104 Wrapper__int32 = Wrapper__int32{
        value: t103,
    }
    var wrapped__17 Shape__int32 = Wrapped{
        _0: t104,
    }
    var t105 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t105,
    }
    var t106 int32 = combined__16.x
    var t107 int32 = combined__16.y
    var t108 int32 = t106 + t107
    var t109 int32 = shape_value(wrapped__17)
    var t110 int32 = t108 + t109
    var t111 int32 = list_value(list__18)
    var t112 int32 = t110 + t111
    var t113 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t112)
    println__T_string(t113)
    return struct{}{}
}

func wrap__T_int32(value__4 int32) int32 {
    var retv116 int32
    var id__6 closure_env_id_1 = closure_env_id_1{}
    var t117 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(id__6, value__4)
    retv116 = t117
    return retv116
}

func println__T_string(value__1 string) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int32_to_string(self__6)
    retv122 = t123
    return retv122
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv125 string
    retv125 = self__38
    return retv125
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env67 closure_env_add_0, value__13 int32) int32 {
    var retv127 int32
    var offset__12 int32 = env67.offset_0
    var t128 int32 = value__13 + offset__12
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env68 closure_env_id_1, item__5 int32) int32 {
    var retv130 int32
    retv130 = item__5
    return retv130
}

func main() {
    main0()
}
