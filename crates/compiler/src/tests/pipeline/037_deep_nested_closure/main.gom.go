package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
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
    var ret11 struct{}
    var a__0 int32 = 10
    var f1__11 closure_env_f1_3 = closure_env_f1_3{
        a_0: a__0,
    }
    var result__12 int32 = _goml_inherent_closure_env_f1_3_closure_env_f1_3_apply(f1__11, 1)
    var t4 string = int32_to_string(result__12)
    ret11 = string_println(t4)
    return ret11
}

func _goml_inherent_closure_env_f4_0_closure_env_f4_0_apply(env0 closure_env_f4_0, w__7 int32) int32 {
    var ret12 int32
    var a__0 int32 = env0.a_0
    var b__2 int32 = env0.b_1
    var c__4 int32 = env0.c_2
    var d__6 int32 = env0.d_3
    var x__1 int32 = env0.x_4
    var y__3 int32 = env0.y_5
    var z__5 int32 = env0.z_6
    var t10 int32 = a__0 + b__2
    var t9 int32 = t10 + c__4
    var t8 int32 = t9 + d__6
    var t7 int32 = t8 + x__1
    var t6 int32 = t7 + y__3
    var t5 int32 = t6 + z__5
    ret12 = t5 + w__7
    return ret12
}

func _goml_inherent_closure_env_f3_1_closure_env_f3_1_apply(env1 closure_env_f3_1, z__5 int32) int32 {
    var ret13 int32
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
    ret13 = _goml_inherent_closure_env_f4_0_closure_env_f4_0_apply(f4__8, 4)
    return ret13
}

func _goml_inherent_closure_env_f2_2_closure_env_f2_2_apply(env2 closure_env_f2_2, y__3 int32) int32 {
    var ret14 int32
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
    ret14 = _goml_inherent_closure_env_f3_1_closure_env_f3_1_apply(f3__9, 3)
    return ret14
}

func _goml_inherent_closure_env_f1_3_closure_env_f1_3_apply(env3 closure_env_f1_3, x__1 int32) int32 {
    var ret15 int32
    var a__0 int32 = env3.a_0
    var b__2 int32 = 20
    var f2__10 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    ret15 = _goml_inherent_closure_env_f2_2_closure_env_f2_2_apply(f2__10, 2)
    return ret15
}

func main() {
    main0()
}
