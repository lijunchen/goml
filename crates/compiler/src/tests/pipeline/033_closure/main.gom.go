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
    var t25 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 2)
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t25)
    println__T_string(t26)
    var t27 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 3)
    var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t27)
    println__T_string(t28)
    return struct{}{}
}

func main0() struct{} {
    var base__6 int32 = 5
    var add_base__8 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var result__9 int32 = _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(add_base__8, 7)
    var printer__13 closure_env_printer_2 = closure_env_printer_2{}
    _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(printer__13, "result: ", result__9)
    var no_capture__17 closure_env_no_capture_4 = closure_env_no_capture_4{}
    var doubled__18 int32 = _goml_m_inherent_i_closure__en_ha32319a3d33750b05233a1c4e08c6ec1_ture__4_i_apply(no_capture__17, 3)
    var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(doubled__18)
    println__T_string(t34)
    test()
    var t35 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t36 IntList = Cons{
        _0: 2,
        _1: t35,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t36,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var play_list_and_point__25 closure_env_play_list_and_point_5 = closure_env_play_list_and_point_5{
        list123_0: list123__19,
        point_1: point__20,
    }
    _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(play_list_and_point__25)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t38 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t38)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv41 string
    var t42 string = _goml_runtime_core_int32_to_string(self__2)
    retv41 = t42
    return retv41
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv44 string
    retv44 = self__9
    return retv44
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env18 closure_env_f_0, x__2 int32) int32 {
    var retv46 int32
    var y__0 int32 = env18.y_0
    var z__1 int32 = env18.z_1
    var t47 int32 = x__2 * y__0
    var t48 int32 = t47 * z__1
    retv46 = t48
    return retv46
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env19 closure_env_add_base_1, x__7 int32) int32 {
    var retv50 int32
    var base__6 int32 = env19.base_0
    var t51 int32 = x__7 + base__6
    retv50 = t51
    return retv50
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env20 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t53 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
    var message__12 string = prefix__10 + t53
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha32319a3d33750b05233a1c4e08c6ec1_ture__4_i_apply(env22 closure_env_no_capture_4, z__16 int32) int32 {
    var retv59 int32
    var t60 int32 = z__16 * 2
    retv59 = t60
    return retv59
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env23 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env23.list123_0
    var point__20 Point = env23.point_1
    switch list123__19.(type) {
    case Nil:
        println__T_string("Empty list")
    case Cons:
        var x11 int32 = list123__19.(Cons)._0
        var head__21 int32 = x11
        var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(head__21)
        println__T_string(t64)
        var x14 int32 = point__20.x
        var x15 int32 = point__20.y
        var y__24 int32 = x15
        var x__23 int32 = x14
        var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
        var t66 string = "Point: (" + t65
        var t67 string = t66 + ", "
        var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
        var t69 string = t67 + t68
        var t70 string = t69 + ")"
        println__T_string(t70)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main() {
    main0()
}
