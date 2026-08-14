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
        var x189 Node = value__10.(Cons)._0
        var t219 int32 = x189.value
        var t220 List = x189.next
        var t221 int32 = list_value(t220)
        var t222 int32 = t219 + t221
        return t222
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t224 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var add__14 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(t224, p0)
    }
    var t225 int32 = add__14(1)
    var point__15 Point
    var inline286 int32 = 3
    var inline287 Point = Point{
        x: t225,
        y: inline286,
    }
    point__15 = inline287
    var t226 Point
    var inline284 Point = Point{
        x: 0,
        y: 0,
    }
    t226 = inline284
    var combined__16 Point
    var inline276 int32 = point__15.x
    var inline277 int32 = t226.x
    var inline278 int32 = inline276 + inline277
    var inline279 int32 = point__15.y
    var inline280 int32 = t226.y
    var inline281 int32 = inline279 + inline280
    var inline282 Point = Point{
        x: inline278,
        y: inline281,
    }
    combined__16 = inline282
    var t227 int32
    var inline271 int32 = 4
    var inline272 closure_env_id_1 = closure_env_id_1{}
    var inline273 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline272, p0)
    }
    var inline274 int32 = inline273(inline271)
    t227 = inline274
    var t229 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t229,
    }
    var t230 int32 = combined__16.x
    var t231 int32 = combined__16.y
    var t232 int32 = t230 + t231
    var t233 int32
    t233 = t227
    var t234 int32 = t232 + t233
    var t235 int32 = list_value(list__18)
    var t236 int32 = t234 + t235
    var t237 string
    var inline260 string = _goml_runtime_core_int32_to_string(t236)
    t237 = inline260
    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline257)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env190 closure_env_add_0, value__13 int32) int32 {
    var offset__12 int32 = env190.offset_0
    var t253 int32 = value__13 + offset__12
    return t253
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env191 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
