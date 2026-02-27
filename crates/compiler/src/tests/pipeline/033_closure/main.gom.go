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
    var ret111 struct{}
    var y__0 int32 = 3
    var z__1 int32 = 5
    var f__3 closure_env_f_0 = closure_env_f_0{
        y_0: y__0,
        z_1: z__1,
    }
    var t95 int32 = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__3, 2)
    var t94 string = int32_to_string(t95)
    string_println(t94)
    var t97 int32 = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__3, 3)
    var t96 string = int32_to_string(t97)
    ret111 = string_println(t96)
    return ret111
}

func main0() struct{} {
    var ret113 struct{}
    var base__6 int32 = 5
    var add_base__8 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var result__9 int32 = _goml_inherent_closure_env_add_base_1_closure_env_add_base_1_apply(add_base__8, 7)
    var printer__13 closure_env_printer_2 = closure_env_printer_2{}
    _goml_inherent_closure_env_printer_2_closure_env_printer_2_apply(printer__13, "result: ", result__9)
    var no_capture__17 closure_env_no_capture_4 = closure_env_no_capture_4{}
    var doubled__18 int32 = _goml_inherent_closure_env_no_capture_4_closure_env_no_capture_4_apply(no_capture__17, 3)
    var t98 string = int32_to_string(doubled__18)
    string_println(t98)
    test()
    var t101 IntList = Nil{}
    var t100 IntList = Cons{
        _0: 3,
        _1: t101,
    }
    var t99 IntList = Cons{
        _0: 2,
        _1: t100,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t99,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var play_list_and_point__25 closure_env_play_list_and_point_5 = closure_env_play_list_and_point_5{
        list123_0: list123__19,
        point_1: point__20,
    }
    _goml_inherent_closure_env_play_list_and_point_5_closure_env_play_list_and_point_5_apply(play_list_and_point__25)
    ret113 = struct{}{}
    return ret113
}

func _goml_inherent_closure_env_f_0_closure_env_f_0_apply(env88 closure_env_f_0, x__2 int32) int32 {
    var ret114 int32
    var y__0 int32 = env88.y_0
    var z__1 int32 = env88.z_1
    var t102 int32 = x__2 * y__0
    ret114 = t102 * z__1
    return ret114
}

func _goml_inherent_closure_env_add_base_1_closure_env_add_base_1_apply(env89 closure_env_add_base_1, x__7 int32) int32 {
    var ret115 int32
    var base__6 int32 = env89.base_0
    ret115 = x__7 + base__6
    return ret115
}

func _goml_inherent_closure_env_printer_2_closure_env_printer_2_apply(env90 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var ret116 struct{}
    var t103 string = int32_to_string(value__11)
    var message__12 string = prefix__10 + t103
    ret116 = string_println(message__12)
    return ret116
}

func _goml_inherent_closure_env_no_capture_4_closure_env_no_capture_4_apply(env92 closure_env_no_capture_4, z__16 int32) int32 {
    var ret118 int32
    ret118 = z__16 * 2
    return ret118
}

func _goml_inherent_closure_env_play_list_and_point_5_closure_env_play_list_and_point_5_apply(env93 closure_env_play_list_and_point_5) struct{} {
    var ret119 struct{}
    var list123__19 IntList = env93.list123_0
    var point__20 Point = env93.point_1
    switch list123__19 := list123__19.(type) {
    case Nil:
        ret119 = string_println("Empty list")
    case Cons:
        var x79 int32 = list123__19._0
        var head__21 int32 = x79
        var t104 string = int32_to_string(head__21)
        string_println(t104)
        var x85 int32 = point__20.x
        var x86 int32 = point__20.y
        var y__24 int32 = x86
        var x__23 int32 = x85
        var t109 string = int32_to_string(x__23)
        var t108 string = "Point: (" + t109
        var t107 string = t108 + ", "
        var t110 string = int32_to_string(y__24)
        var t106 string = t107 + t110
        var t105 string = t106 + ")"
        string_println(t105)
        ret119 = struct{}{}
    }
    return ret119
}

func main() {
    main0()
}
