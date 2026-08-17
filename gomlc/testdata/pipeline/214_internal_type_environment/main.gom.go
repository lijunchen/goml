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

type Ordering int32

type List interface {
    isList()
}

type Cons struct {
    _0 Node
}

func (_ Cons) isList() {}

type Nil struct {}

func (_ Nil) isList() {}

type Shape__int32 struct {
    _tag int32
    _v0_0 Point
    _v1_0 Wrapper__int32
}

func list_value(value__10 List) int32 {
    switch value__10.(type) {
    case Cons:
        var x410 Node = value__10.(Cons)._0
        var t440 int32 = x410.value
        var t441 List = x410.next
        var t442 int32 = list_value(t441)
        var t443 int32 = t440 + t442
        return t443
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t445 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var add__14 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(t445, p0)
    }
    var t446 int32 = add__14(1)
    var point__15 Point
    var inline507 int32 = 3
    var inline508 Point = Point{
        x: t446,
        y: inline507,
    }
    point__15 = inline508
    var t447 Point
    var inline505 Point = Point{
        x: 0,
        y: 0,
    }
    t447 = inline505
    var combined__16 Point
    var inline497 int32 = point__15.x
    var inline498 int32 = t447.x
    var inline499 int32 = inline497 + inline498
    var inline500 int32 = point__15.y
    var inline501 int32 = t447.y
    var inline502 int32 = inline500 + inline501
    var inline503 Point = Point{
        x: inline499,
        y: inline502,
    }
    combined__16 = inline503
    var t448 int32
    var inline492 int32 = 4
    var inline493 closure_env_id_1 = closure_env_id_1{}
    var inline494 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline493, p0)
    }
    var inline495 int32 = inline494(inline492)
    t448 = inline495
    var t450 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t450,
    }
    var t451 int32 = combined__16.x
    var t452 int32 = combined__16.y
    var t453 int32 = t451 + t452
    var t454 int32
    t454 = t448
    var t455 int32 = t453 + t454
    var t456 int32 = list_value(list__18)
    var t457 int32 = t455 + t456
    var t458 string
    var inline481 string = _goml_runtime_core_int32_to_string(t457)
    t458 = inline481
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t458)
    _goml_runtime_core_string_println(inline478)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env411 closure_env_add_0, value__13 int32) int32 {
    var offset__12 int32 = env411.offset_0
    var t474 int32 = value__13 + offset__12
    return t474
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env412 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
