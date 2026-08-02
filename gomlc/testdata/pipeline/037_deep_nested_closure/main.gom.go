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
    var f1__11 closure_env_f1_3 = closure_env_f1_3{
        a_0: a__0,
    }
    var result__12 int32 = _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(f1__11, 1)
    println__T_int32(result__12)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t162 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t162)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t166 string = _goml_runtime_core_int32_to_string(self__43)
    return t166
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env155 closure_env_f4_0, w__7 int32) int32 {
    var a__0 int32 = env155.a_0
    var b__2 int32 = env155.b_1
    var c__4 int32 = env155.c_2
    var d__6 int32 = env155.d_3
    var x__1 int32 = env155.x_4
    var y__3 int32 = env155.y_5
    var z__5 int32 = env155.z_6
    var t169 int32 = a__0 + b__2
    var t170 int32 = t169 + c__4
    var t171 int32 = t170 + d__6
    var t172 int32 = t171 + x__1
    var t173 int32 = t172 + y__3
    var t174 int32 = t173 + z__5
    var t175 int32 = t174 + w__7
    return t175
}

func _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(env156 closure_env_f3_1, z__5 int32) int32 {
    var a__0 int32 = env156.a_0
    var b__2 int32 = env156.b_1
    var c__4 int32 = env156.c_2
    var x__1 int32 = env156.x_3
    var y__3 int32 = env156.y_4
    var d__6 int32 = 40
    var f4__8 closure_env_f4_0 = closure_env_f4_0{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        d_3: d__6,
        x_4: x__1,
        y_5: y__3,
        z_6: z__5,
    }
    var t178 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(f4__8, 4)
    return t178
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env157 closure_env_f2_2, y__3 int32) int32 {
    var a__0 int32 = env157.a_0
    var b__2 int32 = env157.b_1
    var x__1 int32 = env157.x_2
    var c__4 int32 = 30
    var f3__9 closure_env_f3_1 = closure_env_f3_1{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        x_3: x__1,
        y_4: y__3,
    }
    var t181 int32 = _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(f3__9, 3)
    return t181
}

func _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(env158 closure_env_f1_3, x__1 int32) int32 {
    var a__0 int32 = env158.a_0
    var b__2 int32 = 20
    var f2__10 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    var t184 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(f2__10, 2)
    return t184
}

func main() {
    main0()
}
