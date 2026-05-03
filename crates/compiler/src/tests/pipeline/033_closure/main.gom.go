package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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

type closure_env_unused_3 struct {
    result_0 int32
}

type closure_env_no_capture_4 struct {}

type closure_env_play_list_and_point_5 struct {
    list123_0 IntList
    point_1 Point
}

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

func test() struct{} {
    var y__0 int32 = 3
    var z__1 int32 = 5
    var f__3 closure_env_f_0 = closure_env_f_0{
        y_0: y__0,
        z_1: z__1,
    }
    var t18 int32 = _goml_inherent_x23_closure_x5f_env_x5f_f_x5f_0_x23_closure_x5f_env_x5f_f_x5f_0_x23_apply(f__3, 2)
    var t19 string = int32_to_string(t18)
    println__T_string(t19)
    var t20 int32 = _goml_inherent_x23_closure_x5f_env_x5f_f_x5f_0_x23_closure_x5f_env_x5f_f_x5f_0_x23_apply(f__3, 3)
    var t21 string = int32_to_string(t20)
    println__T_string(t21)
    return struct{}{}
}

func main0() struct{} {
    var base__6 int32 = 5
    var add_base__8 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var result__9 int32 = _goml_inherent_x23_closure_x5f_env_x5f_add_x5f_base_x5f_1_x23_closure_x5f_env_x5f_add_x5f_base_x5f_1_x23_apply(add_base__8, 7)
    var printer__13 closure_env_printer_2 = closure_env_printer_2{}
    _goml_inherent_x23_closure_x5f_env_x5f_printer_x5f_2_x23_closure_x5f_env_x5f_printer_x5f_2_x23_apply(printer__13, "result: ", result__9)
    var no_capture__17 closure_env_no_capture_4 = closure_env_no_capture_4{}
    var doubled__18 int32 = _goml_inherent_x23_closure_x5f_env_x5f_no_x5f_capture_x5f_4_x23_closure_x5f_env_x5f_no_x5f_capture_x5f_4_x23_apply(no_capture__17, 3)
    var t27 string = int32_to_string(doubled__18)
    println__T_string(t27)
    test()
    var t28 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t29 IntList = Cons{
        _0: 2,
        _1: t28,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t29,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var play_list_and_point__25 closure_env_play_list_and_point_5 = closure_env_play_list_and_point_5{
        list123_0: list123__19,
        point_1: point__20,
    }
    _goml_inherent_x23_closure_x5f_env_x5f_play_x5f_list_x5f_and_x5f_point_x5f_5_x23_closure_x5f_env_x5f_play_x5f_list_x5f_and_x5f_point_x5f_5_x23_apply(play_list_and_point__25)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_f_x5f_0_x23_closure_x5f_env_x5f_f_x5f_0_x23_apply(env11 closure_env_f_0, x__2 int32) int32 {
    var retv33 int32
    var y__0 int32 = env11.y_0
    var z__1 int32 = env11.z_1
    var t34 int32 = x__2 * y__0
    var t35 int32 = t34 * z__1
    retv33 = t35
    return retv33
}

func _goml_inherent_x23_closure_x5f_env_x5f_add_x5f_base_x5f_1_x23_closure_x5f_env_x5f_add_x5f_base_x5f_1_x23_apply(env12 closure_env_add_base_1, x__7 int32) int32 {
    var retv37 int32
    var base__6 int32 = env12.base_0
    var t38 int32 = x__7 + base__6
    retv37 = t38
    return retv37
}

func _goml_inherent_x23_closure_x5f_env_x5f_printer_x5f_2_x23_closure_x5f_env_x5f_printer_x5f_2_x23_apply(env13 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t40 string = int32_to_string(value__11)
    var message__12 string = prefix__10 + t40
    println__T_string(message__12)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_no_x5f_capture_x5f_4_x23_closure_x5f_env_x5f_no_x5f_capture_x5f_4_x23_apply(env15 closure_env_no_capture_4, z__16 int32) int32 {
    var retv46 int32
    var t47 int32 = z__16 * 2
    retv46 = t47
    return retv46
}

func _goml_inherent_x23_closure_x5f_env_x5f_play_x5f_list_x5f_and_x5f_point_x5f_5_x23_closure_x5f_env_x5f_play_x5f_list_x5f_and_x5f_point_x5f_5_x23_apply(env16 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env16.list123_0
    var point__20 Point = env16.point_1
    switch list123__19.(type) {
    case Nil:
        println__T_string("Empty list")
    case Cons:
        var x4 int32 = list123__19.(Cons)._0
        var head__21 int32 = x4
        var t51 string = int32_to_string(head__21)
        println__T_string(t51)
        var x7 int32 = point__20.x
        var x8 int32 = point__20.y
        var y__24 int32 = x8
        var x__23 int32 = x7
        var t52 string = int32_to_string(x__23)
        var t53 string = "Point: (" + t52
        var t54 string = t53 + ", "
        var t55 string = int32_to_string(y__24)
        var t56 string = t54 + t55
        var t57 string = t56 + ")"
        println__T_string(t57)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main() {
    main0()
}
