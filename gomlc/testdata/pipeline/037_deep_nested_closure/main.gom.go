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

type closure_env_f4_0 struct {
    a_0 int32
    b_1 int32
    c_2 int32
    d_3 int32
    x_4 int32
    y_5 int32
    z_6 int32
}

type closure_env_f3_1 struct {
    a_0 int32
    b_1 int32
    c_2 int32
    x_3 int32
    y_4 int32
}

type closure_env_f2_2 struct {
    a_0 int32
    b_1 int32
    x_2 int32
}

type closure_env_f1_3 struct {
    a_0 int32
}

func main0() struct{} {
    var a__0 int32 = 10
    var result__12 int32
    var inline211 int32 = 1
    var inline213 int32 = 20
    var inline214 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: inline213,
        x_2: inline211,
    }
    var inline215 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(inline214, 2)
    result__12 = inline215
    var inline208 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(result__12)
    _goml_runtime_core_string_println(inline208)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t188 string = _goml_runtime_core_int32_to_string(self__72)
    return t188
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env177 closure_env_f4_0, w__7 int32) int32 {
    var a__0 int32 = env177.a_0
    var b__2 int32 = env177.b_1
    var c__4 int32 = env177.c_2
    var d__6 int32 = env177.d_3
    var x__1 int32 = env177.x_4
    var y__3 int32 = env177.y_5
    var z__5 int32 = env177.z_6
    var t191 int32 = a__0 + b__2
    var t192 int32 = t191 + c__4
    var t193 int32 = t192 + d__6
    var t194 int32 = t193 + x__1
    var t195 int32 = t194 + y__3
    var t196 int32 = t195 + z__5
    var t197 int32 = t196 + w__7
    return t197
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env179 closure_env_f2_2, y__3 int32) int32 {
    var a__0 int32 = env179.a_0
    var b__2 int32 = env179.b_1
    var x__1 int32 = env179.x_2
    var c__4 int32 = 30
    var inline235 int32 = 3
    var inline241 int32 = 40
    var inline242 closure_env_f4_0 = closure_env_f4_0{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        d_3: inline241,
        x_4: x__1,
        y_5: y__3,
        z_6: inline235,
    }
    var inline243 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(inline242, 4)
    return inline243
}

func main() {
    main0()
}
