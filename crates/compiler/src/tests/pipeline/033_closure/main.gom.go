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
    var t22 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 2)
    var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t22)
    println__T_string(t23)
    var t24 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 3)
    var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t24)
    println__T_string(t25)
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
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(doubled__18)
    println__T_string(t31)
    test()
    var t32 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t33 IntList = Cons{
        _0: 2,
        _1: t32,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t33,
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
    var t35 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv38 string
    var t39 string = _goml_runtime_core_int32_to_string(self__2)
    retv38 = t39
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv41 string
    retv41 = self__9
    return retv41
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env15 closure_env_f_0, x__2 int32) int32 {
    var retv43 int32
    var y__0 int32 = env15.y_0
    var z__1 int32 = env15.z_1
    var t44 int32 = x__2 * y__0
    var t45 int32 = t44 * z__1
    retv43 = t45
    return retv43
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env16 closure_env_add_base_1, x__7 int32) int32 {
    var retv47 int32
    var base__6 int32 = env16.base_0
    var t48 int32 = x__7 + base__6
    retv47 = t48
    return retv47
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env17 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t50 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
    var message__12 string = prefix__10 + t50
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha32319a3d33750b05233a1c4e08c6ec1_ture__4_i_apply(env19 closure_env_no_capture_4, z__16 int32) int32 {
    var retv56 int32
    var t57 int32 = z__16 * 2
    retv56 = t57
    return retv56
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env20 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env20.list123_0
    var point__20 Point = env20.point_1
    switch list123__19.(type) {
    case Nil:
        println__T_string("Empty list")
    case Cons:
        var x8 int32 = list123__19.(Cons)._0
        var head__21 int32 = x8
        var t61 string = _goml_m_inherent_i_int32_i_int32_i_to__string(head__21)
        println__T_string(t61)
        var x11 int32 = point__20.x
        var x12 int32 = point__20.y
        var y__24 int32 = x12
        var x__23 int32 = x11
        var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
        var t63 string = "Point: (" + t62
        var t64 string = t63 + ", "
        var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
        var t66 string = t64 + t65
        var t67 string = t66 + ")"
        println__T_string(t67)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main() {
    main0()
}
