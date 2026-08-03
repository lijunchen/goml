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
    var inline246 int32 = 7
    var inline248 int32 = inline246 + base__6
    result__9 = inline248
    var inline241 string = "result: "
    var inline242 string = _goml_m_inherent_i_int32_i_int32_i_to__string(result__9)
    var inline243 string = inline241 + inline242
    println__T_string(inline243)
    var doubled__18 int32
    var inline238 int32 = 3
    var inline239 int32 = inline238 * 2
    doubled__18 = inline239
    var t163 string
    var inline236 string = _goml_runtime_core_int32_to_string(doubled__18)
    t163 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline233)
    var inline223 int32 = 3
    var inline224 int32 = 5
    var inline225 closure_env_f_0 = closure_env_f_0{
        y_0: inline223,
        z_1: inline224,
    }
    var inline226 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline225, 2)
    var inline227 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
    println__T_string(inline227)
    var inline229 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline225, 3)
    var inline230 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline229)
    println__T_string(inline230)
    var t164 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t165 IntList = Cons{
        _0: 2,
        _1: t164,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t165,
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
    var t167 string
    t167 = value__31
    _goml_runtime_core_string_println(t167)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t171 string = _goml_runtime_core_int32_to_string(self__35)
    return t171
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env147 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env147.y_0
    var z__1 int32 = env147.z_1
    var t176 int32 = x__2 * y__0
    var t177 int32 = t176 * z__1
    return t177
}

func _goml_m_inherent_i_closure__en_h53641d12fbb745a3fbb1e4782f631152_oint__5_i_apply(env152 closure_env_play_list_and_point_5) struct{} {
    var list123__19 IntList = env152.list123_0
    var point__20 Point = env152.point_1
    switch list123__19.(type) {
    case Nil:
        var inline256 string = "Empty list"
        var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline256)
        _goml_runtime_core_string_println(inline257)
        return struct{}{}
    case Cons:
        var x140 int32 = list123__19.(Cons)._0
        var t193 string
        var inline270 string = _goml_runtime_core_int32_to_string(x140)
        t193 = inline270
        var inline267 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
        _goml_runtime_core_string_println(inline267)
        var x143 int32 = point__20.x
        var x144 int32 = point__20.y
        var t194 string
        var inline265 string = _goml_runtime_core_int32_to_string(x143)
        t194 = inline265
        var t195 string = "Point: (" + t194
        var t196 string = t195 + ", "
        var t197 string
        var inline263 string = _goml_runtime_core_int32_to_string(x144)
        t197 = inline263
        var t198 string = t196 + t197
        var t199 string = t198 + ")"
        var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
        _goml_runtime_core_string_println(inline260)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
