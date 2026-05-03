package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
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
    var result__12 int32 = _goml_inherent_x23_closure_x5f_env_x5f_f1_x5f_3_x23_closure_x5f_env_x5f_f1_x5f_3_x23_apply(f1__11, 1)
    println__T_int32(result__12)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t7 string = int32_to_string(value__1)
    string_println(t7)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_f4_x5f_0_x23_closure_x5f_env_x5f_f4_x5f_0_x23_apply(env0 closure_env_f4_0, w__7 int32) int32 {
    var retv10 int32
    var a__0 int32 = env0.a_0
    var b__2 int32 = env0.b_1
    var c__4 int32 = env0.c_2
    var d__6 int32 = env0.d_3
    var x__1 int32 = env0.x_4
    var y__3 int32 = env0.y_5
    var z__5 int32 = env0.z_6
    var t11 int32 = a__0 + b__2
    var t12 int32 = t11 + c__4
    var t13 int32 = t12 + d__6
    var t14 int32 = t13 + x__1
    var t15 int32 = t14 + y__3
    var t16 int32 = t15 + z__5
    var t17 int32 = t16 + w__7
    retv10 = t17
    return retv10
}

func _goml_inherent_x23_closure_x5f_env_x5f_f3_x5f_1_x23_closure_x5f_env_x5f_f3_x5f_1_x23_apply(env1 closure_env_f3_1, z__5 int32) int32 {
    var retv19 int32
    var a__0 int32 = env1.a_0
    var b__2 int32 = env1.b_1
    var c__4 int32 = env1.c_2
    var x__1 int32 = env1.x_3
    var y__3 int32 = env1.y_4
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
    var t20 int32 = _goml_inherent_x23_closure_x5f_env_x5f_f4_x5f_0_x23_closure_x5f_env_x5f_f4_x5f_0_x23_apply(f4__8, 4)
    retv19 = t20
    return retv19
}

func _goml_inherent_x23_closure_x5f_env_x5f_f2_x5f_2_x23_closure_x5f_env_x5f_f2_x5f_2_x23_apply(env2 closure_env_f2_2, y__3 int32) int32 {
    var retv22 int32
    var a__0 int32 = env2.a_0
    var b__2 int32 = env2.b_1
    var x__1 int32 = env2.x_2
    var c__4 int32 = 30
    var f3__9 closure_env_f3_1 = closure_env_f3_1{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        x_3: x__1,
        y_4: y__3,
    }
    var t23 int32 = _goml_inherent_x23_closure_x5f_env_x5f_f3_x5f_1_x23_closure_x5f_env_x5f_f3_x5f_1_x23_apply(f3__9, 3)
    retv22 = t23
    return retv22
}

func _goml_inherent_x23_closure_x5f_env_x5f_f1_x5f_3_x23_closure_x5f_env_x5f_f1_x5f_3_x23_apply(env3 closure_env_f1_3, x__1 int32) int32 {
    var retv25 int32
    var a__0 int32 = env3.a_0
    var b__2 int32 = 20
    var f2__10 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    var t26 int32 = _goml_inherent_x23_closure_x5f_env_x5f_f2_x5f_2_x23_closure_x5f_env_x5f_f2_x5f_2_x23_apply(f2__10, 2)
    retv25 = t26
    return retv25
}

func main() {
    main0()
}
