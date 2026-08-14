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
    var t209 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var add_base__8 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(t209, p0)
    }
    var result__9 int32 = add_base__8(7)
    var t210 closure_env_printer_2 = closure_env_printer_2{}
    var printer__13 func(string, int32) struct{} = func(p0 string, p1 int32) struct{} {
        return _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(t210, p0, p1)
    }
    printer__13("result: ", result__9)
    var t211 closure_env_no_capture_3 = closure_env_no_capture_3{}
    var no_capture__17 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(t211, p0)
    }
    var doubled__18 int32 = no_capture__17(3)
    var t212 string
    var inline272 string = _goml_runtime_core_int32_to_string(doubled__18)
    t212 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline269)
    var inline258 int32 = 3
    var inline259 int32 = 5
    var inline260 closure_env_f_0 = closure_env_f_0{
        y_0: inline258,
        z_1: inline259,
    }
    var inline261 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline260, p0)
    }
    var inline262 int32 = inline261(2)
    var inline263 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline262)
    println__T_string(inline263)
    var inline265 int32 = inline261(3)
    var inline266 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline265)
    println__T_string(inline266)
    var t213 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t214 IntList = Cons{
        _0: 2,
        _1: t213,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t214,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var t215 closure_env_play_list_and_point_4 = closure_env_play_list_and_point_4{
        list123_0: list123__19,
        point_1: point__20,
    }
    var play_list_and_point__25 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(t215)
    }
    play_list_and_point__25()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t217 string
    t217 = value__1
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t221 string = _goml_runtime_core_int32_to_string(self__33)
    return t221
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env193 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env193.y_0
    var z__1 int32 = env193.z_1
    var t226 int32 = x__2 * y__0
    var t227 int32 = t226 * z__1
    return t227
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env194 closure_env_add_base_1, x__7 int32) int32 {
    var base__6 int32 = env194.base_0
    var t230 int32 = x__7 + base__6
    return t230
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env195 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t232 string
    var inline278 string = _goml_runtime_core_int32_to_string(value__11)
    t232 = inline278
    var message__12 string = prefix__10 + t232
    var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline275)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(env196 closure_env_no_capture_3, z__16 int32) int32 {
    var t236 int32 = z__16 * 2
    return t236
}

func _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(env197 closure_env_play_list_and_point_4) struct{} {
    var list123__19 IntList = env197.list123_0
    var point__20 Point = env197.point_1
    switch list123__19.(type) {
    case Nil:
        var inline280 string = "Empty list"
        var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline280)
        _goml_runtime_core_string_println(inline281)
        return struct{}{}
    case Cons:
        var x186 int32 = list123__19.(Cons)._0
        var t240 string
        var inline294 string = _goml_runtime_core_int32_to_string(x186)
        t240 = inline294
        var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
        _goml_runtime_core_string_println(inline291)
        var x189 int32 = point__20.x
        var x190 int32 = point__20.y
        var t241 string
        var inline289 string = _goml_runtime_core_int32_to_string(x189)
        t241 = inline289
        var t242 string = "Point: (" + t241
        var t243 string = t242 + ", "
        var t244 string
        var inline287 string = _goml_runtime_core_int32_to_string(x190)
        t244 = inline287
        var t245 string = t243 + t244
        var t246 string = t245 + ")"
        var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t246)
        _goml_runtime_core_string_println(inline284)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
