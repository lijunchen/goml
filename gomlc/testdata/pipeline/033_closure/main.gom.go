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
    var t214 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var add_base__8 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(t214, p0)
    }
    var result__9 int32 = add_base__8(7)
    var t215 closure_env_printer_2 = closure_env_printer_2{}
    var printer__13 func(string, int32) struct{} = func(p0 string, p1 int32) struct{} {
        return _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(t215, p0, p1)
    }
    printer__13("result: ", result__9)
    var t216 closure_env_no_capture_3 = closure_env_no_capture_3{}
    var no_capture__17 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(t216, p0)
    }
    var doubled__18 int32 = no_capture__17(3)
    var t217 string
    var inline277 string = _goml_runtime_core_int32_to_string(doubled__18)
    t217 = inline277
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline274)
    var inline263 int32 = 3
    var inline264 int32 = 5
    var inline265 closure_env_f_0 = closure_env_f_0{
        y_0: inline263,
        z_1: inline264,
    }
    var inline266 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline265, p0)
    }
    var inline267 int32 = inline266(2)
    var inline268 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline267)
    println__T_string(inline268)
    var inline270 int32 = inline266(3)
    var inline271 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline270)
    println__T_string(inline271)
    var t218 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t219 IntList = Cons{
        _0: 2,
        _1: t218,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t219,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var t220 closure_env_play_list_and_point_4 = closure_env_play_list_and_point_4{
        list123_0: list123__19,
        point_1: point__20,
    }
    var play_list_and_point__25 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(t220)
    }
    play_list_and_point__25()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t222 string
    t222 = value__1
    _goml_runtime_core_string_println(t222)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t226 string = _goml_runtime_core_int32_to_string(self__33)
    return t226
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env198 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env198.y_0
    var z__1 int32 = env198.z_1
    var t231 int32 = x__2 * y__0
    var t232 int32 = t231 * z__1
    return t232
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env199 closure_env_add_base_1, x__7 int32) int32 {
    var base__6 int32 = env199.base_0
    var t235 int32 = x__7 + base__6
    return t235
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env200 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t237 string
    var inline283 string = _goml_runtime_core_int32_to_string(value__11)
    t237 = inline283
    var message__12 string = prefix__10 + t237
    var inline280 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline280)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(env201 closure_env_no_capture_3, z__16 int32) int32 {
    var t241 int32 = z__16 * 2
    return t241
}

func _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(env202 closure_env_play_list_and_point_4) struct{} {
    var list123__19 IntList = env202.list123_0
    var point__20 Point = env202.point_1
    switch list123__19.(type) {
    case Nil:
        var inline285 string = "Empty list"
        var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline285)
        _goml_runtime_core_string_println(inline286)
        return struct{}{}
    case Cons:
        var x191 int32 = list123__19.(Cons)._0
        var t245 string
        var inline299 string = _goml_runtime_core_int32_to_string(x191)
        t245 = inline299
        var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t245)
        _goml_runtime_core_string_println(inline296)
        var x194 int32 = point__20.x
        var x195 int32 = point__20.y
        var t246 string
        var inline294 string = _goml_runtime_core_int32_to_string(x194)
        t246 = inline294
        var t247 string = "Point: (" + t246
        var t248 string = t247 + ", "
        var t249 string
        var inline292 string = _goml_runtime_core_int32_to_string(x195)
        t249 = inline292
        var t250 string = t248 + t249
        var t251 string = t250 + ")"
        var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
        _goml_runtime_core_string_println(inline289)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
