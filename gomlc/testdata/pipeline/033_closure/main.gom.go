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
    var t126 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 2)
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t126)
    println__T_string(t127)
    var t128 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 3)
    var t129 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t128)
    println__T_string(t129)
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
    var t135 string = _goml_m_inherent_i_int32_i_int32_i_to__string(doubled__18)
    println__T_string(t135)
    test()
    var t136 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t137 IntList = Cons{
        _0: 2,
        _1: t136,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t137,
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
    var t139 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t139)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv142 string
    var t143 string = _goml_runtime_core_int32_to_string(self__6)
    retv142 = t143
    return retv142
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv145 string
    retv145 = self__38
    return retv145
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env119 closure_env_f_0, x__2 int32) int32 {
    var retv147 int32
    var y__0 int32 = env119.y_0
    var z__1 int32 = env119.z_1
    var t148 int32 = x__2 * y__0
    var t149 int32 = t148 * z__1
    retv147 = t149
    return retv147
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env120 closure_env_add_base_1, x__7 int32) int32 {
    var retv151 int32
    var base__6 int32 = env120.base_0
    var t152 int32 = x__7 + base__6
    retv151 = t152
    return retv151
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env121 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t154 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
    var message__12 string = prefix__10 + t154
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha32319a3d33750b05233a1c4e08c6ec1_ture__4_i_apply(env123 closure_env_no_capture_4, z__16 int32) int32 {
    var retv160 int32
    var t161 int32 = z__16 * 2
    retv160 = t161
    return retv160
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env124 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env124.list123_0
    var point__20 Point = env124.point_1
    switch list123__19.(type) {
    case Nil:
        println__T_string("Empty list")
    case Cons:
        var x112 int32 = list123__19.(Cons)._0
        var head__21 int32 = x112
        var t165 string = _goml_m_inherent_i_int32_i_int32_i_to__string(head__21)
        println__T_string(t165)
        var x115 int32 = point__20.x
        var x116 int32 = point__20.y
        var y__24 int32 = x116
        var x__23 int32 = x115
        var t166 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__23)
        var t167 string = "Point: (" + t166
        var t168 string = t167 + ", "
        var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__24)
        var t170 string = t168 + t169
        var t171 string = t170 + ")"
        println__T_string(t171)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main() {
    main0()
}
