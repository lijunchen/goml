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
    var inline282 int32 = 7
    var inline284 int32 = inline282 + base__6
    result__9 = inline284
    var inline277 string = "result: "
    var inline278 string = _goml_m_inherent_i_int32_i_int32_i_to__string(result__9)
    var inline279 string = inline277 + inline278
    println__T_string(inline279)
    var doubled__18 int32
    var inline274 int32 = 3
    var inline275 int32 = inline274 * 2
    doubled__18 = inline275
    var t199 string
    var inline272 string = _goml_runtime_core_int32_to_string(doubled__18)
    t199 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline269)
    var inline259 int32 = 3
    var inline260 int32 = 5
    var inline261 closure_env_f_0 = closure_env_f_0{
        y_0: inline259,
        z_1: inline260,
    }
    var inline262 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline261, 2)
    var inline263 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline262)
    println__T_string(inline263)
    var inline265 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline261, 3)
    var inline266 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline265)
    println__T_string(inline266)
    var t200 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t201 IntList = Cons{
        _0: 2,
        _1: t200,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t201,
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
    var t203 string
    t203 = value__31
    _goml_runtime_core_string_println(t203)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t207 string = _goml_runtime_core_int32_to_string(self__35)
    return t207
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env183 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env183.y_0
    var z__1 int32 = env183.z_1
    var t212 int32 = x__2 * y__0
    var t213 int32 = t212 * z__1
    return t213
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env188 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env188.list123_0
    var point__20 Point = env188.point_1
    switch list123__19.(type) {
    case Nil:
        var inline292 string = "Empty list"
        var inline293 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline292)
        _goml_runtime_core_string_println(inline293)
        return struct{}{}
    case Cons:
        var x176 int32 = list123__19.(Cons)._0
        var t229 string
        var inline306 string = _goml_runtime_core_int32_to_string(x176)
        t229 = inline306
        var inline303 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t229)
        _goml_runtime_core_string_println(inline303)
        var x179 int32 = point__20.x
        var x180 int32 = point__20.y
        var t230 string
        var inline301 string = _goml_runtime_core_int32_to_string(x179)
        t230 = inline301
        var t231 string = "Point: (" + t230
        var t232 string = t231 + ", "
        var t233 string
        var inline299 string = _goml_runtime_core_int32_to_string(x180)
        t233 = inline299
        var t234 string = t232 + t233
        var t235 string = t234 + ")"
        var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
        _goml_runtime_core_string_println(inline296)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
