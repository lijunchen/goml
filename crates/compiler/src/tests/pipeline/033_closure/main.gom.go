package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
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
    var y__0 int32
    var z__1 int32
    var f__3 closure_env_f_0
    var t17 int32
    var t18 string
    var t19 int32
    var t20 string
    var t21 struct{}
    y__0 = 3
    z__1 = 5
    f__3 = closure_env_f_0{
        y_0: y__0,
        z_1: z__1,
    }
    t17 = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__3, 2)
    t18 = int32_to_string(t17)
    string_println(t18)
    t19 = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__3, 3)
    t20 = int32_to_string(t19)
    t21 = string_println(t20)
    return t21
}

func main0() struct{} {
    var base__6 int32
    var add_base__8 closure_env_add_base_1
    var result__9 int32
    var printer__13 closure_env_printer_2
    var no_capture__17 closure_env_no_capture_4
    var doubled__18 int32
    var t23 string
    var t24 IntList
    var t25 IntList
    var list123__19 IntList
    var point__20 Point
    var play_list_and_point__25 closure_env_play_list_and_point_5
    base__6 = 5
    add_base__8 = closure_env_add_base_1{
        base_0: base__6,
    }
    result__9 = _goml_inherent_closure_env_add_base_1_closure_env_add_base_1_apply(add_base__8, 7)
    printer__13 = closure_env_printer_2{}
    _goml_inherent_closure_env_printer_2_closure_env_printer_2_apply(printer__13, "result: ", result__9)
    no_capture__17 = closure_env_no_capture_4{}
    doubled__18 = _goml_inherent_closure_env_no_capture_4_closure_env_no_capture_4_apply(no_capture__17, 3)
    t23 = int32_to_string(doubled__18)
    string_println(t23)
    test()
    t24 = Cons{
        _0: 3,
        _1: Nil{},
    }
    t25 = Cons{
        _0: 2,
        _1: t24,
    }
    list123__19 = Cons{
        _0: 1,
        _1: t25,
    }
    point__20 = Point{
        x: 10,
        y: 20,
    }
    play_list_and_point__25 = closure_env_play_list_and_point_5{
        list123_0: list123__19,
        point_1: point__20,
    }
    _goml_inherent_closure_env_play_list_and_point_5_closure_env_play_list_and_point_5_apply(play_list_and_point__25)
    return struct{}{}
}

func _goml_inherent_closure_env_f_0_closure_env_f_0_apply(env11 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32
    var z__1 int32
    var t26 int32
    var t27 int32
    y__0 = env11.y_0
    z__1 = env11.z_1
    t26 = x__2 * y__0
    t27 = t26 * z__1
    return t27
}

func _goml_inherent_closure_env_add_base_1_closure_env_add_base_1_apply(env12 closure_env_add_base_1, x__7 int32) int32 {
    var base__6 int32
    var t28 int32
    base__6 = env12.base_0
    t28 = x__7 + base__6
    return t28
}

func _goml_inherent_closure_env_printer_2_closure_env_printer_2_apply(env13 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t29 string
    var message__12 string
    var t30 struct{}
    t29 = int32_to_string(value__11)
    message__12 = prefix__10 + t29
    t30 = string_println(message__12)
    return t30
}

func _goml_inherent_closure_env_no_capture_4_closure_env_no_capture_4_apply(env15 closure_env_no_capture_4, z__16 int32) int32 {
    var t32 int32
    t32 = z__16 * 2
    return t32
}

func _goml_inherent_closure_env_play_list_and_point_5_closure_env_play_list_and_point_5_apply(env16 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList
    var point__20 Point
    var x4 int32
    var x5 IntList
    var head__21 int32
    var t35 string
    var x7 int32
    var x8 int32
    var y__24 int32
    var x__23 int32
    var t36 string
    var t37 string
    var t38 string
    var t39 string
    var t40 string
    var t41 string
    list123__19 = env16.list123_0
    point__20 = env16.point_1
    switch list123__19.(type) {
    case Nil:
        goto b2
    case Cons:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return struct{}{}
    b2:
    string_println("Empty list")
    goto b1
    b3:
    x4 = list123__19.(Cons)._0
    x5 = list123__19.(Cons)._1
    head__21 = x4
    t35 = int32_to_string(head__21)
    string_println(t35)
    x7 = point__20.x
    x8 = point__20.y
    y__24 = x8
    x__23 = x7
    t36 = int32_to_string(x__23)
    t37 = "Point: (" + t36
    t38 = t37 + ", "
    t39 = int32_to_string(y__24)
    t40 = t38 + t39
    t41 = t40 + ")"
    string_println(t41)
    goto b1
}

func main() {
    main0()
}
