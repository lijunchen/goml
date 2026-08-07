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
    var inline206 int32 = 1
    var inline208 int32 = 20
    var inline209 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: inline208,
        x_2: inline206,
    }
    var inline210 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(inline209, 2)
    result__12 = inline210
    var inline203 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(result__12)
    _goml_runtime_core_string_println(inline203)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t183 string = _goml_runtime_core_int32_to_string(self__72)
    return t183
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env172 closure_env_f4_0, w__7 int32) int32 {
    var a__0 int32 = env172.a_0
    var b__2 int32 = env172.b_1
    var c__4 int32 = env172.c_2
    var d__6 int32 = env172.d_3
    var x__1 int32 = env172.x_4
    var y__3 int32 = env172.y_5
    var z__5 int32 = env172.z_6
    var t186 int32 = a__0 + b__2
    var t187 int32 = t186 + c__4
    var t188 int32 = t187 + d__6
    var t189 int32 = t188 + x__1
    var t190 int32 = t189 + y__3
    var t191 int32 = t190 + z__5
    var t192 int32 = t191 + w__7
    return t192
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env174 closure_env_f2_2, y__3 int32) int32 {
    var a__0 int32 = env174.a_0
    var b__2 int32 = env174.b_1
    var x__1 int32 = env174.x_2
    var c__4 int32 = 30
    var inline230 int32 = 3
    var inline236 int32 = 40
    var inline237 closure_env_f4_0 = closure_env_f4_0{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        d_3: inline236,
        x_4: x__1,
        y_5: y__3,
        z_6: inline230,
    }
    var inline238 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(inline237, 4)
    return inline238
}

func main() {
    main0()
}
