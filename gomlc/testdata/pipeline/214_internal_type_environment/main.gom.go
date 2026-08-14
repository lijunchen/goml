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
        var x184 Node = value__10.(Cons)._0
        var t214 int32 = x184.value
        var t215 List = x184.next
        var t216 int32 = list_value(t215)
        var t217 int32 = t214 + t216
        return t217
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t219 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var add__14 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(t219, p0)
    }
    var t220 int32 = add__14(1)
    var point__15 Point
    var inline281 int32 = 3
    var inline282 Point = Point{
        x: t220,
        y: inline281,
    }
    point__15 = inline282
    var t221 Point
    var inline279 Point = Point{
        x: 0,
        y: 0,
    }
    t221 = inline279
    var combined__16 Point
    var inline271 int32 = point__15.x
    var inline272 int32 = t221.x
    var inline273 int32 = inline271 + inline272
    var inline274 int32 = point__15.y
    var inline275 int32 = t221.y
    var inline276 int32 = inline274 + inline275
    var inline277 Point = Point{
        x: inline273,
        y: inline276,
    }
    combined__16 = inline277
    var t222 int32
    var inline266 int32 = 4
    var inline267 closure_env_id_1 = closure_env_id_1{}
    var inline268 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline267, p0)
    }
    var inline269 int32 = inline268(inline266)
    t222 = inline269
    var t224 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t224,
    }
    var t225 int32 = combined__16.x
    var t226 int32 = combined__16.y
    var t227 int32 = t225 + t226
    var t228 int32
    t228 = t222
    var t229 int32 = t227 + t228
    var t230 int32 = list_value(list__18)
    var t231 int32 = t229 + t230
    var t232 string
    var inline255 string = _goml_runtime_core_int32_to_string(t231)
    t232 = inline255
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline252)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env185 closure_env_add_0, value__13 int32) int32 {
    var offset__12 int32 = env185.offset_0
    var t248 int32 = value__13 + offset__12
    return t248
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env186 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
