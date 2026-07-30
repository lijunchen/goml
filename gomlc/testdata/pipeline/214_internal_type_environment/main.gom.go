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
    var retv74 Point
    var t75 Point = Point{
        x: x__0,
        y: y__1,
    }
    retv74 = t75
    return retv74
}

func _goml_m_inherent_i_Point_i_Point_i_copy(self__2 Point, other__3 Point) Point {
    var retv77 Point
    var t78 int32 = self__2.x
    var t79 int32 = other__3.x
    var t80 int32 = t78 + t79
    var t81 int32 = self__2.y
    var t82 int32 = other__3.y
    var t83 int32 = t81 + t82
    var t84 Point = Point{
        x: t80,
        y: t83,
    }
    retv77 = t84
    return retv77
}

func _goml_m_inherent_i_Point_i_Point_i_origin() Point {
    var retv86 Point
    var t87 Point = Point{
        x: 0,
        y: 0,
    }
    retv86 = t87
    return retv86
}

func shape_value(value__7 Shape__int32) int32 {
    var retv89 int32
    var jp91 int32
    switch value__7.(type) {
    case Dot:
        var x68 Point = value__7.(Dot)._0
        var point__8 Point = x68
        var t92 int32 = point__8.x
        var t93 int32 = point__8.y
        var t94 int32 = t92 + t93
        jp91 = t94
    case Wrapped:
        var x69 Wrapper__int32 = value__7.(Wrapped)._0
        var wrapper__9 Wrapper__int32 = x69
        var t95 int32 = wrapper__9.value
        jp91 = t95
    case Origin:
        jp91 = 0
    default:
        panic("non-exhaustive match")
    }
    retv89 = jp91
    return retv89
}

func list_value(value__10 List) int32 {
    var retv97 int32
    var jp99 int32
    switch value__10.(type) {
    case Cons:
        var x70 Node = value__10.(Cons)._0
        var node__11 Node = x70
        var t100 int32 = node__11.value
        var t101 List = node__11.next
        var t102 int32 = list_value(t101)
        var t103 int32 = t100 + t102
        jp99 = t103
    case Nil:
        jp99 = 0
    default:
        panic("non-exhaustive match")
    }
    retv97 = jp99
    return retv97
}

func main0() struct{} {
    var offset__12 int32 = 1
    var add__14 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var t105 int32 = _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(add__14, 1)
    var point__15 Point = _goml_m_inherent_i_Point_i_Point_i_new(t105, 3)
    var t106 Point = _goml_m_inherent_i_Point_i_Point_i_origin()
    var combined__16 Point = _goml_m_inherent_i_Point_i_Point_i_copy(point__15, t106)
    var t107 int32 = wrap__T_int32(4)
    var t108 Wrapper__int32 = Wrapper__int32{
        value: t107,
    }
    var wrapped__17 Shape__int32 = Wrapped{
        _0: t108,
    }
    var t109 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t109,
    }
    var t110 int32 = combined__16.x
    var t111 int32 = combined__16.y
    var t112 int32 = t110 + t111
    var t113 int32 = shape_value(wrapped__17)
    var t114 int32 = t112 + t113
    var t115 int32 = list_value(list__18)
    var t116 int32 = t114 + t115
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t116)
    println__T_string(t117)
    return struct{}{}
}

func wrap__T_int32(value__4 int32) int32 {
    var retv120 int32
    var id__6 closure_env_id_1 = closure_env_id_1{}
    var t121 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(id__6, value__4)
    retv120 = t121
    return retv120
}

func println__T_string(value__1 string) struct{} {
    var t123 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t123)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int32_to_string(self__6)
    retv126 = t127
    return retv126
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv129 string
    retv129 = self__38
    return retv129
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env71 closure_env_add_0, value__13 int32) int32 {
    var retv131 int32
    var offset__12 int32 = env71.offset_0
    var t132 int32 = value__13 + offset__12
    retv131 = t132
    return retv131
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env72 closure_env_id_1, item__5 int32) int32 {
    var retv134 int32
    retv134 = item__5
    return retv134
}

func main() {
    main0()
}
