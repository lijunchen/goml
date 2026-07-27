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
    var t82 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 2)
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t82)
    println__T_string(t83)
    var t84 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 3)
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    println__T_string(t85)
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
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(doubled__18)
    println__T_string(t91)
    test()
    var t92 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t93 IntList = Cons{
        _0: 2,
        _1: t92,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t93,
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
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv98 string
    var t99 string = _goml_runtime_core_int32_to_string(self__6)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv101 string
    retv101 = self__38
    return retv101
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env75 closure_env_f_0, x__2 int32) int32 {
    var retv103 int32
    var y__0 int32 = env75.y_0
    var z__1 int32 = env75.z_1
    var t104 int32 = x__2 * y__0
    var t105 int32 = t104 * z__1
    retv103 = t105
    return retv103
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env76 closure_env_add_base_1, x__7 int32) int32 {
    var retv107 int32
    var base__6 int32 = env76.base_0
    var t108 int32 = x__7 + base__6
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env77 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t110 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
    var message__12 string = prefix__10 + t110
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha32319a3d33750b05233a1c4e08c6ec1_ture__4_i_apply(env79 closure_env_no_capture_4, z__16 int32) int32 {
    var retv116 int32
    var t117 int32 = z__16 * 2
    retv116 = t117
    return retv116
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env80 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env80.list123_0
    var point__20 Point = env80.point_1
    switch list123__19.(type) {
    case Nil:
        println__T_string("Empty list")
    case Cons:
        var x68 int32 = list123__19.(Cons)._0
        var head__21 int32 = x68
        var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(head__21)
        println__T_string(t121)
        var x71 int32 = point__20.x
        var x72 int32 = point__20.y
        var y__24 int32 = x72
        var x__23 int32 = x71
        var t122 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
        var t123 string = "Point: (" + t122
        var t124 string = t123 + ", "
        var t125 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
        var t126 string = t124 + t125
        var t127 string = t126 + ")"
        println__T_string(t127)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main() {
    main0()
}
