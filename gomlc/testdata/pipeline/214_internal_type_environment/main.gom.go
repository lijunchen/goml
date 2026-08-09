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
    var t209 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var add__14 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(t209, p0)
    }
    var t210 int32 = add__14(1)
    var point__15 Point
    var inline271 int32 = 3
    var inline272 Point = Point{
        x: t210,
        y: inline271,
    }
    point__15 = inline272
    var t211 Point
    var inline269 Point = Point{
        x: 0,
        y: 0,
    }
    t211 = inline269
    var combined__16 Point
    var inline261 int32 = point__15.x
    var inline262 int32 = t211.x
    var inline263 int32 = inline261 + inline262
    var inline264 int32 = point__15.y
    var inline265 int32 = t211.y
    var inline266 int32 = inline264 + inline265
    var inline267 Point = Point{
        x: inline263,
        y: inline266,
    }
    combined__16 = inline267
    var t212 int32
    var inline256 int32 = 4
    var inline257 closure_env_id_1 = closure_env_id_1{}
    var inline258 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline257, p0)
    }
    var inline259 int32 = inline258(inline256)
    t212 = inline259
    var t214 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t214,
    }
    var t215 int32 = combined__16.x
    var t216 int32 = combined__16.y
    var t217 int32 = t215 + t216
    var t218 int32
    t218 = t212
    var t219 int32 = t217 + t218
    var t220 int32 = list_value(list__18)
    var t221 int32 = t219 + t220
    var t222 string
    var inline245 string = _goml_runtime_core_int32_to_string(t221)
    t222 = inline245
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline242)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env175 closure_env_add_0, value__13 int32) int32 {
    var offset__12 int32 = env175.offset_0
    var t238 int32 = value__13 + offset__12
    return t238
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env176 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
