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
    var ret34 struct{}
    var y__0 int32 = 3
    var z__1 int32 = 5
    var f__3 closure_env_f_0 = closure_env_f_0{
        y_0: y__0,
        z_1: z__1,
    }
    var t18 int32 = impl_inherent_closure_env_f_0_apply(f__3, 2)
    var t17 string = int32_to_string(t18)
    string_println(t17)
    var t20 int32 = impl_inherent_closure_env_f_0_apply(f__3, 3)
    var t19 string = int32_to_string(t20)
    ret34 = string_println(t19)
    return ret34
}

func main0() struct{} {
    var ret36 struct{}
    var base__6 int32 = 5
    var add_base__8 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var result__9 int32 = impl_inherent_closure_env_add_base_1_apply(add_base__8, 7)
    var printer__13 closure_env_printer_2 = closure_env_printer_2{}
    impl_inherent_closure_env_printer_2_apply(printer__13, "result: ", result__9)
    var no_capture__17 closure_env_no_capture_4 = closure_env_no_capture_4{}
    var doubled__18 int32 = impl_inherent_closure_env_no_capture_4_apply(no_capture__17, 3)
    var t21 string = int32_to_string(doubled__18)
    string_println(t21)
    test()
    var t24 IntList = Nil{}
    var t23 IntList = Cons{
        _0: 3,
        _1: t24,
    }
    var t22 IntList = Cons{
        _0: 2,
        _1: t23,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t22,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var play_list_and_point__25 closure_env_play_list_and_point_5 = closure_env_play_list_and_point_5{
        list123_0: list123__19,
        point_1: point__20,
    }
    impl_inherent_closure_env_play_list_and_point_5_apply(play_list_and_point__25)
    ret36 = struct{}{}
    return ret36
}

func impl_inherent_closure_env_f_0_apply(env11 closure_env_f_0, x__2 int32) int32 {
    var ret37 int32
    var y__0 int32 = env11.y_0
    var z__1 int32 = env11.z_1
    var t25 int32 = x__2 * y__0
    ret37 = t25 * z__1
    return ret37
}

func impl_inherent_closure_env_add_base_1_apply(env12 closure_env_add_base_1, x__7 int32) int32 {
    var ret38 int32
    var base__6 int32 = env12.base_0
    ret38 = x__7 + base__6
    return ret38
}

func impl_inherent_closure_env_printer_2_apply(env13 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var ret39 struct{}
    var t26 string = int32_to_string(value__11)
    var message__12 string = prefix__10 + t26
    ret39 = string_println(message__12)
    return ret39
}

func impl_inherent_closure_env_no_capture_4_apply(env15 closure_env_no_capture_4, z__16 int32) int32 {
    var ret41 int32
    ret41 = z__16 * 2
    return ret41
}

func impl_inherent_closure_env_play_list_and_point_5_apply(env16 closure_env_play_list_and_point_5) struct{} {
    var ret42 struct{}
    var list123__19 IntList = env16.list123_0
    var point__20 Point = env16.point_1
    switch list123__19 := list123__19.(type) {
    case Nil:
        ret42 = string_println("Empty list")
    case Cons:
        var x4 int32 = list123__19._0
        var head__21 int32 = x4
        var t27 string = int32_to_string(head__21)
        string_println(t27)
        var x7 int32 = point__20.x
        var x8 int32 = point__20.y
        var y__24 int32 = x8
        var x__23 int32 = x7
        var t32 string = int32_to_string(x__23)
        var t31 string = "Point: (" + t32
        var t30 string = t31 + ", "
        var t33 string = int32_to_string(y__24)
        var t29 string = t30 + t33
        var t28 string = t29 + ")"
        string_println(t28)
        ret42 = struct{}{}
    }
    return ret42
}

func main() {
    main0()
}
