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
    var t11 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t11)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv14 string
    var t15 string = _goml_runtime_core_int32_to_string(self__13)
    retv14 = t15
    return retv14
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env4 closure_env_f4_0, w__7 int32) int32 {
    var retv17 int32
    var a__0 int32 = env4.a_0
    var b__2 int32 = env4.b_1
    var c__4 int32 = env4.c_2
    var d__6 int32 = env4.d_3
    var x__1 int32 = env4.x_4
    var y__3 int32 = env4.y_5
    var z__5 int32 = env4.z_6
    var t18 int32 = a__0 + b__2
    var t19 int32 = t18 + c__4
    var t20 int32 = t19 + d__6
    var t21 int32 = t20 + x__1
    var t22 int32 = t21 + y__3
    var t23 int32 = t22 + z__5
    var t24 int32 = t23 + w__7
    retv17 = t24
    return retv17
}

func _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(env5 closure_env_f3_1, z__5 int32) int32 {
    var retv26 int32
    var a__0 int32 = env5.a_0
    var b__2 int32 = env5.b_1
    var c__4 int32 = env5.c_2
    var x__1 int32 = env5.x_3
    var y__3 int32 = env5.y_4
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
    var t27 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(f4__8, 4)
    retv26 = t27
    return retv26
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env6 closure_env_f2_2, y__3 int32) int32 {
    var retv29 int32
    var a__0 int32 = env6.a_0
    var b__2 int32 = env6.b_1
    var x__1 int32 = env6.x_2
    var c__4 int32 = 30
    var f3__9 closure_env_f3_1 = closure_env_f3_1{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        x_3: x__1,
        y_4: y__3,
    }
    var t30 int32 = _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(f3__9, 3)
    retv29 = t30
    return retv29
}

func _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(env7 closure_env_f1_3, x__1 int32) int32 {
    var retv32 int32
    var a__0 int32 = env7.a_0
    var b__2 int32 = 20
    var f2__10 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    var t33 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(f2__10, 2)
    retv32 = t33
    return retv32
}

func main() {
    main0()
}
