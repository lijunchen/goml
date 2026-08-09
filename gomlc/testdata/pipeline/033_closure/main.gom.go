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
    var result__9 int32
    var inline278 int32 = 7
    var inline280 int32 = inline278 + base__6
    result__9 = inline280
    var inline273 string = "result: "
    var inline274 string = _goml_m_inherent_i_int32_i_int32_i_to__string(result__9)
    var inline275 string = inline273 + inline274
    println__T_string(inline275)
    var doubled__18 int32
    var inline270 int32 = 3
    var inline271 int32 = inline270 * 2
    doubled__18 = inline271
    var t198 string
    var inline268 string = _goml_runtime_core_int32_to_string(doubled__18)
    t198 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline265)
    var inline255 int32 = 3
    var inline256 int32 = 5
    var inline257 closure_env_f_0 = closure_env_f_0{
        y_0: inline255,
        z_1: inline256,
    }
    var inline258 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline257, 2)
    var inline259 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline258)
    println__T_string(inline259)
    var inline261 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline257, 3)
    var inline262 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline261)
    println__T_string(inline262)
    var t199 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t200 IntList = Cons{
        _0: 2,
        _1: t199,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t200,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var play_list_and_point__25 closure_env_play_list_and_point_4 = closure_env_play_list_and_point_4{
        list123_0: list123__19,
        point_1: point__20,
    }
    _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(play_list_and_point__25)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t202 string
    t202 = value__31
    _goml_runtime_core_string_println(t202)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t206 string = _goml_runtime_core_int32_to_string(self__35)
    return t206
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env183 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env183.y_0
    var z__1 int32 = env183.z_1
    var t211 int32 = x__2 * y__0
    var t212 int32 = t211 * z__1
    return t212
}

func _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(env187 closure_env_play_list_and_point_4) struct{} {
    var list123__19 IntList = env187.list123_0
    var point__20 Point = env187.point_1
    switch list123__19.(type) {
    case Nil:
        var inline288 string = "Empty list"
        var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline288)
        _goml_runtime_core_string_println(inline289)
        return struct{}{}
    case Cons:
        var x176 int32 = list123__19.(Cons)._0
        var t225 string
        var inline302 string = _goml_runtime_core_int32_to_string(x176)
        t225 = inline302
        var inline299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
        _goml_runtime_core_string_println(inline299)
        var x179 int32 = point__20.x
        var x180 int32 = point__20.y
        var t226 string
        var inline297 string = _goml_runtime_core_int32_to_string(x179)
        t226 = inline297
        var t227 string = "Point: (" + t226
        var t228 string = t227 + ", "
        var t229 string
        var inline295 string = _goml_runtime_core_int32_to_string(x180)
        t229 = inline295
        var t230 string = t228 + t229
        var t231 string = t230 + ")"
        var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
        _goml_runtime_core_string_println(inline292)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
