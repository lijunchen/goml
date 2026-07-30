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
    var t115 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv118 string
    var t119 string = _goml_runtime_core_int32_to_string(self__43)
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env108 closure_env_f4_0, w__7 int32) int32 {
    var retv121 int32
    var a__0 int32 = env108.a_0
    var b__2 int32 = env108.b_1
    var c__4 int32 = env108.c_2
    var d__6 int32 = env108.d_3
    var x__1 int32 = env108.x_4
    var y__3 int32 = env108.y_5
    var z__5 int32 = env108.z_6
    var t122 int32 = a__0 + b__2
    var t123 int32 = t122 + c__4
    var t124 int32 = t123 + d__6
    var t125 int32 = t124 + x__1
    var t126 int32 = t125 + y__3
    var t127 int32 = t126 + z__5
    var t128 int32 = t127 + w__7
    retv121 = t128
    return retv121
}

func _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(env109 closure_env_f3_1, z__5 int32) int32 {
    var retv130 int32
    var a__0 int32 = env109.a_0
    var b__2 int32 = env109.b_1
    var c__4 int32 = env109.c_2
    var x__1 int32 = env109.x_3
    var y__3 int32 = env109.y_4
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
    var t131 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(f4__8, 4)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env110 closure_env_f2_2, y__3 int32) int32 {
    var retv133 int32
    var a__0 int32 = env110.a_0
    var b__2 int32 = env110.b_1
    var x__1 int32 = env110.x_2
    var c__4 int32 = 30
    var f3__9 closure_env_f3_1 = closure_env_f3_1{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        x_3: x__1,
        y_4: y__3,
    }
    var t134 int32 = _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(f3__9, 3)
    retv133 = t134
    return retv133
}

func _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(env111 closure_env_f1_3, x__1 int32) int32 {
    var retv136 int32
    var a__0 int32 = env111.a_0
    var b__2 int32 = 20
    var f2__10 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    var t137 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(f2__10, 2)
    retv136 = t137
    return retv136
}

func main() {
    main0()
}
