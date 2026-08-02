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
    var inline265 int32 = 7
    var inline267 int32 = inline265 + base__6
    result__9 = inline267
    var inline260 string = "result: "
    var inline261 string = _goml_m_inherent_i_int32_i_int32_i_to__string(result__9)
    var inline262 string = inline260 + inline261
    println__T_string(inline262)
    var doubled__18 int32
    var inline257 int32 = 3
    var inline258 int32 = inline257 * 2
    doubled__18 = inline258
    var t182 string
    var inline255 string = _goml_runtime_core_int32_to_string(doubled__18)
    t182 = inline255
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline252)
    var inline242 int32 = 3
    var inline243 int32 = 5
    var inline244 closure_env_f_0 = closure_env_f_0{
        y_0: inline242,
        z_1: inline243,
    }
    var inline245 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, 2)
    var inline246 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline245)
    println__T_string(inline246)
    var inline248 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, 3)
    var inline249 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline248)
    println__T_string(inline249)
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
    var t186 string
    t186 = value__1
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

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env171 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env171.list123_0
    var point__20 Point = env171.point_1
    switch list123__19.(type) {
    case Nil:
        var inline275 string = "Empty list"
        var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline275)
        _goml_runtime_core_string_println(inline276)
        return struct{}{}
    case Cons:
        var x159 int32 = list123__19.(Cons)._0
        var t212 string
        var inline289 string = _goml_runtime_core_int32_to_string(x159)
        t212 = inline289
        var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
        _goml_runtime_core_string_println(inline286)
        var x162 int32 = point__20.x
        var x163 int32 = point__20.y
        var t213 string
        var inline284 string = _goml_runtime_core_int32_to_string(x162)
        t213 = inline284
        var t214 string = "Point: (" + t213
        var t215 string = t214 + ", "
        var t216 string
        var inline282 string = _goml_runtime_core_int32_to_string(x163)
        t216 = inline282
        var t217 string = t215 + t216
        var t218 string = t217 + ")"
        var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t218)
        _goml_runtime_core_string_println(inline279)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
