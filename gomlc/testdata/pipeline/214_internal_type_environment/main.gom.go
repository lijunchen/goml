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
        var x179 Node = value__10.(Cons)._0
        var t209 int32 = x179.value
        var t210 List = x179.next
        var t211 int32 = list_value(t210)
        var t212 int32 = t209 + t211
        return t212
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t214 int32
    var inline276 int32 = 1
    var inline278 int32 = inline276 + offset__12
    t214 = inline278
    var point__15 Point
    var inline273 int32 = 3
    var inline274 Point = Point{
        x: t214,
        y: inline273,
    }
    point__15 = inline274
    var t215 Point
    var inline271 Point = Point{
        x: 0,
        y: 0,
    }
    t215 = inline271
    var combined__16 Point
    var inline263 int32 = point__15.x
    var inline264 int32 = t215.x
    var inline265 int32 = inline263 + inline264
    var inline266 int32 = point__15.y
    var inline267 int32 = t215.y
    var inline268 int32 = inline266 + inline267
    var inline269 Point = Point{
        x: inline265,
        y: inline268,
    }
    combined__16 = inline269
    var t216 int32
    var inline259 int32 = 4
    var inline260 closure_env_id_1 = closure_env_id_1{}
    var inline261 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline260, inline259)
    t216 = inline261
    var t218 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t218,
    }
    var t219 int32 = combined__16.x
    var t220 int32 = combined__16.y
    var t221 int32 = t219 + t220
    var t222 int32
    t222 = t216
    var t223 int32 = t221 + t222
    var t224 int32 = list_value(list__18)
    var t225 int32 = t223 + t224
    var t226 string
    var inline248 string = _goml_runtime_core_int32_to_string(t225)
    t226 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env181 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
