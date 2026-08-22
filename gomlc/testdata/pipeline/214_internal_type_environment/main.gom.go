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

type Wrapper__i32 struct {
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

type Shape__i32 struct {
    _tag int32
    _v0_0 Point
    _v1_0 Wrapper__i32
}

func list_value(value__10 List) int32 {
    switch value__10.(type) {
    case Cons:
        var x413 Node = value__10.(Cons)._0
        var t443 int32 = x413.value
        var t444 List = x413.next
        var t445 int32 = list_value(t444)
        var t446 int32 = t443 + t445
        return t446
    case Nil:
        return 0
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var offset__12 int32 = 1
    var t448 closure_env_add_0 = closure_env_add_0{
        offset_0: offset__12,
    }
    var add__14 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(t448, p0)
    }
    var t449 int32 = add__14(1)
    var point__15 Point
    var inline510 int32 = 3
    var inline511 Point = Point{
        x: t449,
        y: inline510,
    }
    point__15 = inline511
    var t450 Point
    var inline508 Point = Point{
        x: 0,
        y: 0,
    }
    t450 = inline508
    var combined__16 Point
    var inline500 int32 = point__15.x
    var inline501 int32 = t450.x
    var inline502 int32 = inline500 + inline501
    var inline503 int32 = point__15.y
    var inline504 int32 = t450.y
    var inline505 int32 = inline503 + inline504
    var inline506 Point = Point{
        x: inline502,
        y: inline505,
    }
    combined__16 = inline506
    var t451 int32
    var inline495 int32 = 4
    var inline496 closure_env_id_1 = closure_env_id_1{}
    var inline497 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(inline496, p0)
    }
    var inline498 int32 = inline497(inline495)
    t451 = inline498
    var t453 Node = Node{
        value: 5,
        next: Nil{},
    }
    var list__18 List = Cons{
        _0: t453,
    }
    var t454 int32 = combined__16.x
    var t455 int32 = combined__16.y
    var t456 int32 = t454 + t455
    var t457 int32
    t457 = t451
    var t458 int32 = t456 + t457
    var t459 int32 = list_value(list__18)
    var t460 int32 = t458 + t459
    var t461 string
    var inline484 string = _goml_runtime_core_int32_to_string(t460)
    t461 = inline484
    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t461)
    _goml_runtime_core_string_println(inline481)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__add__0_i_closure__env__add__0_i_apply(env414 closure_env_add_0, value__13 int32) int32 {
    var offset__12 int32 = env414.offset_0
    var t477 int32 = value__13 + offset__12
    return t477
}

func _goml_m_inherent_i_closure__env__id__1_i_closure__env__id__1_i_apply(env415 closure_env_id_1, item__5 int32) int32 {
    return item__5
}

func main() {
    main0()
}
