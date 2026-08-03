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
    var inline170 int32 = 1
    var inline172 int32 = 20
    var inline173 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: inline172,
        x_2: inline170,
    }
    var inline174 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(inline173, 2)
    result__12 = inline174
    var inline167 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(result__12)
    _goml_runtime_core_string_println(inline167)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t147 string = _goml_runtime_core_int32_to_string(self__72)
    return t147
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env136 closure_env_f4_0, w__7 int32) int32 {
    var a__0 int32 = env136.a_0
    var b__2 int32 = env136.b_1
    var c__4 int32 = env136.c_2
    var d__6 int32 = env136.d_3
    var x__1 int32 = env136.x_4
    var y__3 int32 = env136.y_5
    var z__5 int32 = env136.z_6
    var t150 int32 = a__0 + b__2
    var t151 int32 = t150 + c__4
    var t152 int32 = t151 + d__6
    var t153 int32 = t152 + x__1
    var t154 int32 = t153 + y__3
    var t155 int32 = t154 + z__5
    var t156 int32 = t155 + w__7
    return t156
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env138 closure_env_f2_2, y__3 int32) int32 {
    var a__0 int32 = env138.a_0
    var b__2 int32 = env138.b_1
    var x__1 int32 = env138.x_2
    var c__4 int32 = 30
    var inline194 int32 = 3
    var inline200 int32 = 40
    var inline201 closure_env_f4_0 = closure_env_f4_0{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        d_3: inline200,
        x_4: x__1,
        y_5: y__3,
        z_6: inline194,
    }
    var inline202 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(inline201, 4)
    return inline202
}

func main() {
    main0()
}
