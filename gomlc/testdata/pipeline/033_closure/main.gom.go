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

func main0() struct{} {
    var base__6 int32 = 5
    var result__9 int32
    var inline287 int32 = 7
    var inline289 int32 = inline287 + base__6
    result__9 = inline289
    var inline282 string = "result: "
    var inline283 string = _goml_m_inherent_i_int32_i_int32_i_to__string(result__9)
    var inline284 string = inline282 + inline283
    println__T_string(inline284)
    var doubled__18 int32
    var inline279 int32 = 3
    var inline280 int32 = inline279 * 2
    doubled__18 = inline280
    var t204 string
    var inline277 string = _goml_runtime_core_int32_to_string(doubled__18)
    t204 = inline277
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline274)
    var inline264 int32 = 3
    var inline265 int32 = 5
    var inline266 closure_env_f_0 = closure_env_f_0{
        y_0: inline264,
        z_1: inline265,
    }
    var inline267 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline266, 2)
    var inline268 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline267)
    println__T_string(inline268)
    var inline270 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline266, 3)
    var inline271 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline270)
    println__T_string(inline271)
    var t205 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t206 IntList = Cons{
        _0: 2,
        _1: t205,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t206,
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

func println__T_string(value__31 string) struct{} {
    var t208 string
    t208 = value__31
    _goml_runtime_core_string_println(t208)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t212 string = _goml_runtime_core_int32_to_string(self__35)
    return t212
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env188 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env188.y_0
    var z__1 int32 = env188.z_1
    var t217 int32 = x__2 * y__0
    var t218 int32 = t217 * z__1
    return t218
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env193 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env193.list123_0
    var point__20 Point = env193.point_1
    switch list123__19.(type) {
    case Nil:
        var inline297 string = "Empty list"
        var inline298 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline297)
        _goml_runtime_core_string_println(inline298)
        return struct{}{}
    case Cons:
        var x181 int32 = list123__19.(Cons)._0
        var t234 string
        var inline311 string = _goml_runtime_core_int32_to_string(x181)
        t234 = inline311
        var inline308 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
        _goml_runtime_core_string_println(inline308)
        var x184 int32 = point__20.x
        var x185 int32 = point__20.y
        var t235 string
        var inline306 string = _goml_runtime_core_int32_to_string(x184)
        t235 = inline306
        var t236 string = "Point: (" + t235
        var t237 string = t236 + ", "
        var t238 string
        var inline304 string = _goml_runtime_core_int32_to_string(x185)
        t238 = inline304
        var t239 string = t237 + t238
        var t240 string = t239 + ")"
        var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
        _goml_runtime_core_string_println(inline301)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
