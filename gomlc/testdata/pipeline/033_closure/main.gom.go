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
    var t173 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 2)
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t173)
    println__T_string(t174)
    var t175 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, 3)
    var t176 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t175)
    println__T_string(t176)
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
    var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(doubled__18)
    println__T_string(t182)
    test()
    var t183 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t184 IntList = Cons{
        _0: 2,
        _1: t183,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t184,
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
    var t186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t190 string = _goml_runtime_core_int32_to_string(self__6)
    return t190
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env166 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env166.y_0
    var z__1 int32 = env166.z_1
    var t195 int32 = x__2 * y__0
    var t196 int32 = t195 * z__1
    return t196
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env167 closure_env_add_base_1, x__7 int32) int32 {
    var base__6 int32 = env167.base_0
    var t199 int32 = x__7 + base__6
    return t199
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env168 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t201 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__11)
    var message__12 string = prefix__10 + t201
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha32319a3d33750b05233a1c4e08c6ec1_ture__4_i_apply(env170 closure_env_no_capture_4, z__16 int32) int32 {
    var t208 int32 = z__16 * 2
    return t208
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env171 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env171.list123_0
    var point__20 Point = env171.point_1
    switch list123__19.(type) {
    case Nil:
        println__T_string("Empty list")
        return struct{}{}
    case Cons:
        var x159 int32 = list123__19.(Cons)._0
        var t212 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x159)
        println__T_string(t212)
        var x162 int32 = point__20.x
        var x163 int32 = point__20.y
        var t213 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x162)
        var t214 string = "Point: (" + t213
        var t215 string = t214 + ", "
        var t216 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x163)
        var t217 string = t215 + t216
        var t218 string = t217 + ")"
        println__T_string(t218)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
