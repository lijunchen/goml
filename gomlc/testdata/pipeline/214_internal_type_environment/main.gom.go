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
        var x157 Node = value__10.(Cons)._0
        var t187 int32 = x157.value
        var t188 List = x157.next
        var t189 int32 = list_value(t188)
        var t190 int32 = t187 + t189
        return t190
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t192 int32
    var inline254 int32 = 1
    var inline256 int32 = inline254 + offset__12
    t192 = inline256
    var point__15 Point
    var inline251 int32 = 3
    var inline252 Point = Point{
        x: t192,
        y: inline251,
    }
    point__15 = inline252
    var t193 Point
    var inline249 Point = Point{
        x: 0,
        y: 0,
    }
    t193 = inline249
    var combined__16 Point
    var inline241 int32 = point__15.x
    var inline242 int32 = t193.x
    var inline243 int32 = inline241 + inline242
    var inline244 int32 = point__15.y
    var inline245 int32 = t193.y
    var inline246 int32 = inline244 + inline245
    var inline247 Point = Point{
        x: inline243,
        y: inline246,
    }
    combined__16 = inline247
    var t194 int32
    var inline237 int32 = 4
    var inline238 closure_env_id_1 = closure_env_id_1{}
    var inline239 int32 = _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline238, inline237)
    t194 = inline239
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
    var t200 int32
    t200 = t194
    var t201 int32 = t199 + t200
    var t202 int32 = list_value(list__18)
    var t203 int32 = t201 + t202
    var t204 string
    var inline226 string = _goml_runtime_core_int32_to_string(t203)
    t204 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env159 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
