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
    var t75 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__43)
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env68 closure_env_f4_0, w__7 int32) int32 {
    var retv81 int32
    var a__0 int32 = env68.a_0
    var b__2 int32 = env68.b_1
    var c__4 int32 = env68.c_2
    var d__6 int32 = env68.d_3
    var x__1 int32 = env68.x_4
    var y__3 int32 = env68.y_5
    var z__5 int32 = env68.z_6
    var t82 int32 = a__0 + b__2
    var t83 int32 = t82 + c__4
    var t84 int32 = t83 + d__6
    var t85 int32 = t84 + x__1
    var t86 int32 = t85 + y__3
    var t87 int32 = t86 + z__5
    var t88 int32 = t87 + w__7
    retv81 = t88
    return retv81
}

func _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(env69 closure_env_f3_1, z__5 int32) int32 {
    var retv90 int32
    var a__0 int32 = env69.a_0
    var b__2 int32 = env69.b_1
    var c__4 int32 = env69.c_2
    var x__1 int32 = env69.x_3
    var y__3 int32 = env69.y_4
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
    var t91 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(f4__8, 4)
    retv90 = t91
    return retv90
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env70 closure_env_f2_2, y__3 int32) int32 {
    var retv93 int32
    var a__0 int32 = env70.a_0
    var b__2 int32 = env70.b_1
    var x__1 int32 = env70.x_2
    var c__4 int32 = 30
    var f3__9 closure_env_f3_1 = closure_env_f3_1{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        x_3: x__1,
        y_4: y__3,
    }
    var t94 int32 = _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(f3__9, 3)
    retv93 = t94
    return retv93
}

func _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(env71 closure_env_f1_3, x__1 int32) int32 {
    var retv96 int32
    var a__0 int32 = env71.a_0
    var b__2 int32 = 20
    var f2__10 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    var t97 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(f2__10, 2)
    retv96 = t97
    return retv96
}

func main() {
    main0()
}
