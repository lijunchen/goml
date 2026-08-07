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
        var x174 Node = value__10.(Cons)._0
        var t204 int32 = x174.value
        var t205 List = x174.next
        var t206 int32 = list_value(t205)
        var t207 int32 = t204 + t206
        return t207
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t209 int32
    var inline271 int32 = 1
    var inline273 int32 = inline271 + offset__12
    t209 = inline273
    var point__15 Point
    var inline268 int32 = 3
    var inline269 Point = Point{
        x: t209,
        y: inline268,
    }
    point__15 = inline269
    var t210 Point
    var inline266 Point = Point{
        x: 0,
        y: 0,
    }
    t210 = inline266
    var combined__16 Point
    var inline258 int32 = point__15.x
    var inline259 int32 = t210.x
    var inline260 int32 = inline258 + inline259
    var inline261 int32 = point__15.y
    var inline262 int32 = t210.y
    var inline263 int32 = inline261 + inline262
    var inline264 Point = Point{
        x: inline260,
        y: inline263,
    }
    combined__16 = inline264
    var t211 int32
    var inline254 int32 = 4
    var inline255 closure_env_id_1 = closure_env_id_1{}
    var inline256 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline255, inline254)
    t211 = inline256
    var t213 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t213,
    }
    var t214 int32 = combined__16.x
    var t215 int32 = combined__16.y
    var t216 int32 = t214 + t215
    var t217 int32
    t217 = t211
    var t218 int32 = t216 + t217
    var t219 int32 = list_value(list__18)
    var t220 int32 = t218 + t219
    var t221 string
    var inline243 string = _goml_runtime_core_int32_to_string(t220)
    t221 = inline243
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env176 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
