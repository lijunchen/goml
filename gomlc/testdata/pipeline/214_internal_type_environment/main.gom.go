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
    var retv158 Point
    var t159 Point = Point{
        x: x__0,
        y: y__1,
    }
    retv158 = t159
    return retv158
}

func _goml_m_inherent_i_Point_i_Point_i_copy(self__2 Point, other__3 Point) Point {
    var retv161 Point
    var t162 int32 = self__2.x
    var t163 int32 = other__3.x
    var t164 int32 = t162 + t163
    var t165 int32 = self__2.y
    var t166 int32 = other__3.y
    var t167 int32 = t165 + t166
    var t168 Point = Point{
        x: t164,
        y: t167,
    }
    retv161 = t168
    return retv161
}

func _goml_m_inherent_i_Point_i_Point_i_origin() Point {
    var retv170 Point
    var t171 Point = Point{
        x: 0,
        y: 0,
    }
    retv170 = t171
    return retv170
}

func shape_value(value__7 Shape__int32) int32 {
    var retv173 int32
    var jp175 int32
    switch value__7.(type) {
    case Dot:
        var x152 Point = value__7.(Dot)._0
        var point__8 Point = x152
        var t176 int32 = point__8.x
        var t177 int32 = point__8.y
        var t178 int32 = t176 + t177
        jp175 = t178
    case Wrapped:
        var x153 Wrapper__int32 = value__7.(Wrapped)._0
        var wrapper__9 Wrapper__int32 = x153
        var t179 int32 = wrapper__9.value
        jp175 = t179
    case Origin:
        jp175 = 0
    default:
        panic("non-exhaustive match")
    }
    retv173 = jp175
    return retv173
}

func list_value(value__10 List) int32 {
    var retv181 int32
    var jp183 int32
    switch value__10.(type) {
    case Cons:
        var x154 Node = value__10.(Cons)._0
        var node__11 Node = x154
        var t184 int32 = node__11.value
        var t185 List = node__11.next
        var t186 int32 = list_value(t185)
        var t187 int32 = t184 + t186
        jp183 = t187
    case Nil:
        jp183 = 0
    default:
        panic("non-exhaustive match")
    }
    retv181 = jp183
    return retv181
}

func main0() struct{} {
    var offset__12 int32 = 1
    var add__14 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var t189 int32 = _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(add__14, 1)
    var point__15 Point = _goml_m_inherent_i_Point_i_Point_i_new(t189, 3)
    var t190 Point = _goml_m_inherent_i_Point_i_Point_i_origin()
    var combined__16 Point = _goml_m_inherent_i_Point_i_Point_i_copy(point__15, t190)
    var t191 int32 = wrap__T_int32(4)
    var t192 Wrapper__int32 = Wrapper__int32{
        value: t191,
    }
    var wrapped__17 Shape__int32 = Wrapped{
        _0: t192,
    }
    var t193 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t193,
    }
    var t194 int32 = combined__16.x
    var t195 int32 = combined__16.y
    var t196 int32 = t194 + t195
    var t197 int32 = shape_value(wrapped__17)
    var t198 int32 = t196 + t197
    var t199 int32 = list_value(list__18)
    var t200 int32 = t198 + t199
    var t201 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t200)
    println__T_string(t201)
    return struct{}{}
}

func wrap__T_int32(value__4 int32) int32 {
    var retv204 int32
    var id__6 closure_env_id_1 = closure_env_id_1{}
    var t205 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(id__6, value__4)
    retv204 = t205
    return retv204
}

func println__T_string(value__1 string) struct{} {
    var t207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv210 string
    var t211 string = _goml_runtime_core_int32_to_string(self__6)
    retv210 = t211
    return retv210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv213 string
    retv213 = self__38
    return retv213
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env155 closure_env_add_0, value__13 int32) int32 {
    var retv215 int32
    var offset__12 int32 = env155.offset_0
    var t216 int32 = value__13 + offset__12
    retv215 = t216
    return retv215
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env156 closure_env_id_1, item__5 int32) int32 {
    var retv218 int32
    retv218 = item__5
    return retv218
}

func main() {
    main0()
}
