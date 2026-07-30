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
    var t86 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 2)
    var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t86)
    println__T_string(t87)
    var t88 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 3)
    var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t88)
    println__T_string(t89)
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
    var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(doubled__18)
    println__T_string(t95)
    test()
    var t96 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t97 IntList = Cons{
        _0: 2,
        _1: t96,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t97,
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
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv102 string
    var t103 string = _goml_runtime_core_int32_to_string(self__6)
    retv102 = t103
    return retv102
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv105 string
    retv105 = self__38
    return retv105
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env79 closure_env_f_0, x__2 int32) int32 {
    var retv107 int32
    var y__0 int32 = env79.y_0
    var z__1 int32 = env79.z_1
    var t108 int32 = x__2 * y__0
    var t109 int32 = t108 * z__1
    retv107 = t109
    return retv107
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env80 closure_env_add_base_1, x__7 int32) int32 {
    var retv111 int32
    var base__6 int32 = env80.base_0
    var t112 int32 = x__7 + base__6
    retv111 = t112
    return retv111
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env81 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t114 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
    var message__12 string = prefix__10 + t114
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha32319a3d33750b05233a1c4e08c6ec1_ture__4_i_apply(env83 closure_env_no_capture_4, z__16 int32) int32 {
    var retv120 int32
    var t121 int32 = z__16 * 2
    retv120 = t121
    return retv120
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env84 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env84.list123_0
    var point__20 Point = env84.point_1
    switch list123__19.(type) {
    case Nil:
        println__T_string("Empty list")
    case Cons:
        var x72 int32 = list123__19.(Cons)._0
        var head__21 int32 = x72
        var t125 string = _goml_m_inherent_i_int32_i_int32_i_to__string(head__21)
        println__T_string(t125)
        var x75 int32 = point__20.x
        var x76 int32 = point__20.y
        var y__24 int32 = x76
        var x__23 int32 = x75
        var t126 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
        var t127 string = "Point: (" + t126
        var t128 string = t127 + ", "
        var t129 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
        var t130 string = t128 + t129
        var t131 string = t130 + ")"
        println__T_string(t131)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main() {
    main0()
}
