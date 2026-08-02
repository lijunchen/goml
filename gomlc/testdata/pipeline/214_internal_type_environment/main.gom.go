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
    var retv161 Point
    var t162 Point = Point{
        x: x__0,
        y: y__1,
    }
    retv161 = t162
    return retv161
}

func _goml_m_inherent_i_Point_i_Point_i_copy(self__2 Point, other__3 Point) Point {
    var retv164 Point
    var t165 int32 = self__2.x
    var t166 int32 = other__3.x
    var t167 int32 = t165 + t166
    var t168 int32 = self__2.y
    var t169 int32 = other__3.y
    var t170 int32 = t168 + t169
    var t171 Point = Point{
        x: t167,
        y: t170,
    }
    retv164 = t171
    return retv164
}

func _goml_m_inherent_i_Point_i_Point_i_origin() Point {
    var retv173 Point
    var t174 Point = Point{
        x: 0,
        y: 0,
    }
    retv173 = t174
    return retv173
}

func shape_value(value__7 Shape__int32) int32 {
    var retv176 int32
    var jp178 int32
    switch value__7.(type) {
    case Dot:
        var x155 Point = value__7.(Dot)._0
        var point__8 Point = x155
        var t179 int32 = point__8.x
        var t180 int32 = point__8.y
        var t181 int32 = t179 + t180
        jp178 = t181
    case Wrapped:
        var x156 Wrapper__int32 = value__7.(Wrapped)._0
        var wrapper__9 Wrapper__int32 = x156
        var t182 int32 = wrapper__9.value
        jp178 = t182
    case Origin:
        jp178 = 0
    default:
        panic("non-exhaustive match")
    }
    retv176 = jp178
    return retv176
}

func list_value(value__10 List) int32 {
    var retv184 int32
    var jp186 int32
    switch value__10.(type) {
    case Cons:
        var x157 Node = value__10.(Cons)._0
        var node__11 Node = x157
        var t187 int32 = node__11.value
        var t188 List = node__11.next
        var t189 int32 = list_value(t188)
        var t190 int32 = t187 + t189
        jp186 = t190
    case Nil:
        jp186 = 0
    default:
        panic("non-exhaustive match")
    }
    retv184 = jp186
    return retv184
}

func main0() struct{} {
    var offset__12 int32 = 1
    var add__14 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var t192 int32 = _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(add__14, 1)
    var point__15 Point = _goml_m_inherent_i_Point_i_Point_i_new(t192, 3)
    var t193 Point = _goml_m_inherent_i_Point_i_Point_i_origin()
    var combined__16 Point = _goml_m_inherent_i_Point_i_Point_i_copy(point__15, t193)
    var t194 int32 = wrap__T_int32(4)
    var t195 Wrapper__int32 = Wrapper__int32{
        value: t194,
    }
    var wrapped__17 Shape__int32 = Wrapped{
        _0: t195,
    }
    var t196 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t196,
    }
    var t197 int32 = combined__16.x
    var t198 int32 = combined__16.y
    var t199 int32 = t197 + t198
    var t200 int32 = shape_value(wrapped__17)
    var t201 int32 = t199 + t200
    var t202 int32 = list_value(list__18)
    var t203 int32 = t201 + t202
    var t204 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t203)
    println__T_string(t204)
    return struct{}{}
}

func wrap__T_int32(value__4 int32) int32 {
    var retv207 int32
    var id__6 closure_env_id_1 = closure_env_id_1{}
    var t208 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(id__6, value__4)
    retv207 = t208
    return retv207
}

func println__T_string(value__1 string) struct{} {
    var t210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv213 string
    var t214 string = _goml_runtime_core_int32_to_string(self__6)
    retv213 = t214
    return retv213
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv216 string
    retv216 = self__38
    return retv216
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env158 closure_env_add_0, value__13 int32) int32 {
    var retv218 int32
    var offset__12 int32 = env158.offset_0
    var t219 int32 = value__13 + offset__12
    retv218 = t219
    return retv218
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env159 closure_env_id_1, item__5 int32) int32 {
    var retv221 int32
    retv221 = item__5
    return retv221
}

func main() {
    main0()
}
