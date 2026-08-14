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

type closure_env_f_0 struct {
    y_0 int32
    z_1 int32
}

type closure_env_add_base_1 struct {
    base_0 int32
}

type closure_env_printer_2 struct {}

type closure_env_no_capture_3 struct {}

type closure_env_play_list_and_point_4 struct {
    list123_0 IntList
    point_1 Point
}

type Ordering int32

type IntList interface {
    isIntList()
}

type Nil struct {}

func (_ Nil) isIntList() {}

type Cons struct {
    _0 int32
    _1 IntList
}

func (_ Cons) isIntList() {}

func main0() struct{} {
    var base__6 int32 = 5
    var t435 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var add_base__8 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(t435, p0)
    }
    var result__9 int32 = add_base__8(7)
    var t436 closure_env_printer_2 = closure_env_printer_2{}
    var printer__13 func(string, int32) struct{} = func(p0 string, p1 int32) struct{} {
        return _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(t436, p0, p1)
    }
    printer__13("result: ", result__9)
    var t437 closure_env_no_capture_3 = closure_env_no_capture_3{}
    var no_capture__17 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(t437, p0)
    }
    var doubled__18 int32 = no_capture__17(3)
    var t438 string
    var inline498 string = _goml_runtime_core_int32_to_string(doubled__18)
    t438 = inline498
    var inline495 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline495)
    var inline484 int32 = 3
    var inline485 int32 = 5
    var inline486 closure_env_f_0 = closure_env_f_0{
        y_0: inline484,
        z_1: inline485,
    }
    var inline487 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline486, p0)
    }
    var inline488 int32 = inline487(2)
    var inline489 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline488)
    println__T_string(inline489)
    var inline491 int32 = inline487(3)
    var inline492 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline491)
    println__T_string(inline492)
    var t439 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t440 IntList = Cons{
        _0: 2,
        _1: t439,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t440,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var t441 closure_env_play_list_and_point_4 = closure_env_play_list_and_point_4{
        list123_0: list123__19,
        point_1: point__20,
    }
    var play_list_and_point__25 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(t441)
    }
    play_list_and_point__25()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t443 string
    t443 = value__1
    _goml_runtime_core_string_println(t443)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t447 string = _goml_runtime_core_int32_to_string(self__33)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env419 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env419.y_0
    var z__1 int32 = env419.z_1
    var t452 int32 = x__2 * y__0
    var t453 int32 = t452 * z__1
    return t453
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env420 closure_env_add_base_1, x__7 int32) int32 {
    var base__6 int32 = env420.base_0
    var t456 int32 = x__7 + base__6
    return t456
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env421 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t458 string
    var inline504 string = _goml_runtime_core_int32_to_string(value__11)
    t458 = inline504
    var message__12 string = prefix__10 + t458
    var inline501 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline501)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(env422 closure_env_no_capture_3, z__16 int32) int32 {
    var t462 int32 = z__16 * 2
    return t462
}

func _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(env423 closure_env_play_list_and_point_4) struct{} {
    var list123__19 IntList = env423.list123_0
    var point__20 Point = env423.point_1
    switch list123__19.(type) {
    case Nil:
        var inline506 string = "Empty list"
        var inline507 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline506)
        _goml_runtime_core_string_println(inline507)
        return struct{}{}
    case Cons:
        var x412 int32 = list123__19.(Cons)._0
        var t466 string
        var inline520 string = _goml_runtime_core_int32_to_string(x412)
        t466 = inline520
        var inline517 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t466)
        _goml_runtime_core_string_println(inline517)
        var x415 int32 = point__20.x
        var x416 int32 = point__20.y
        var t467 string
        var inline515 string = _goml_runtime_core_int32_to_string(x415)
        t467 = inline515
        var t468 string = "Point: (" + t467
        var t469 string = t468 + ", "
        var t470 string
        var inline513 string = _goml_runtime_core_int32_to_string(x416)
        t470 = inline513
        var t471 string = t469 + t470
        var t472 string = t471 + ")"
        var inline510 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t472)
        _goml_runtime_core_string_println(inline510)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
