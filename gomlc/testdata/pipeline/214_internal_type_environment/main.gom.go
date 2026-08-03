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

func list_value(value__10 List) int32 {
    switch value__10.(type) {
    case Cons:
        var x138 Node = value__10.(Cons)._0
        var t168 int32 = x138.value
        var t169 List = x138.next
        var t170 int32 = list_value(t169)
        var t171 int32 = t168 + t170
        return t171
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t173 int32
    var inline235 int32 = 1
    var inline237 int32 = inline235 + offset__12
    t173 = inline237
    var point__15 Point
    var inline232 int32 = 3
    var inline233 Point = Point{
        x: t173,
        y: inline232,
    }
    point__15 = inline233
    var t174 Point
    var inline230 Point = Point{
        x: 0,
        y: 0,
    }
    t174 = inline230
    var combined__16 Point
    var inline222 int32 = point__15.x
    var inline223 int32 = t174.x
    var inline224 int32 = inline222 + inline223
    var inline225 int32 = point__15.y
    var inline226 int32 = t174.y
    var inline227 int32 = inline225 + inline226
    var inline228 Point = Point{
        x: inline224,
        y: inline227,
    }
    combined__16 = inline228
    var t175 int32
    var inline218 int32 = 4
    var inline219 closure_env_id_1 = closure_env_id_1{}
    var inline220 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline219, inline218)
    t175 = inline220
    var t177 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t177,
    }
    var t178 int32 = combined__16.x
    var t179 int32 = combined__16.y
    var t180 int32 = t178 + t179
    var t181 int32
    t181 = t175
    var t182 int32 = t180 + t181
    var t183 int32 = list_value(list__18)
    var t184 int32 = t182 + t183
    var t185 string
    var inline207 string = _goml_runtime_core_int32_to_string(t184)
    t185 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env140 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
