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

type closure_env_no_capture_3 struct {}

type closure_env_play_list_and_point_4 struct {
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

func main0() struct{} {
    var base__6 int32 = 5
    var t199 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var add_base__8 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(t199, p0)
    }
    var result__9 int32 = add_base__8(7)
    var t200 closure_env_printer_2 = closure_env_printer_2{}
    var printer__13 func(string, int32) struct{} = func(p0 string, p1 int32) struct{} {
        return _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(t200, p0, p1)
    }
    printer__13("result: ", result__9)
    var t201 closure_env_no_capture_3 = closure_env_no_capture_3{}
    var no_capture__17 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(t201, p0)
    }
    var doubled__18 int32 = no_capture__17(3)
    var t202 string
    var inline262 string = _goml_runtime_core_int32_to_string(doubled__18)
    t202 = inline262
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline259)
    var inline248 int32 = 3
    var inline249 int32 = 5
    var inline250 closure_env_f_0 = closure_env_f_0{
        y_0: inline248,
        z_1: inline249,
    }
    var inline251 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline250, p0)
    }
    var inline252 int32 = inline251(2)
    var inline253 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline252)
    println__T_string(inline253)
    var inline255 int32 = inline251(3)
    var inline256 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline255)
    println__T_string(inline256)
    var t203 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t204 IntList = Cons{
        _0: 2,
        _1: t203,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t204,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var t205 closure_env_play_list_and_point_4 = closure_env_play_list_and_point_4{
        list123_0: list123__19,
        point_1: point__20,
    }
    var play_list_and_point__25 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(t205)
    }
    play_list_and_point__25()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t207 string
    t207 = value__1
    _goml_runtime_core_string_println(t207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t211 string = _goml_runtime_core_int32_to_string(self__33)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env183 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env183.y_0
    var z__1 int32 = env183.z_1
    var t216 int32 = x__2 * y__0
    var t217 int32 = t216 * z__1
    return t217
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env184 closure_env_add_base_1, x__7 int32) int32 {
    var base__6 int32 = env184.base_0
    var t220 int32 = x__7 + base__6
    return t220
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env185 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t222 string
    var inline268 string = _goml_runtime_core_int32_to_string(value__11)
    t222 = inline268
    var message__12 string = prefix__10 + t222
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline265)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(env186 closure_env_no_capture_3, z__16 int32) int32 {
    var t226 int32 = z__16 * 2
    return t226
}

func _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(env187 closure_env_play_list_and_point_4) struct{} {
    var list123__19 IntList = env187.list123_0
    var point__20 Point = env187.point_1
    switch list123__19.(type) {
    case Nil:
        var inline270 string = "Empty list"
        var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline270)
        _goml_runtime_core_string_println(inline271)
        return struct{}{}
    case Cons:
        var x176 int32 = list123__19.(Cons)._0
        var t230 string
        var inline284 string = _goml_runtime_core_int32_to_string(x176)
        t230 = inline284
        var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
        _goml_runtime_core_string_println(inline281)
        var x179 int32 = point__20.x
        var x180 int32 = point__20.y
        var t231 string
        var inline279 string = _goml_runtime_core_int32_to_string(x179)
        t231 = inline279
        var t232 string = "Point: (" + t231
        var t233 string = t232 + ", "
        var t234 string
        var inline277 string = _goml_runtime_core_int32_to_string(x180)
        t234 = inline277
        var t235 string = t233 + t234
        var t236 string = t235 + ")"
        var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
        _goml_runtime_core_string_println(inline274)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
