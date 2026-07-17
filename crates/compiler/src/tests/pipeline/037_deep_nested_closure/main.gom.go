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
    var t65 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv68 string
    var t69 string = _goml_runtime_core_int32_to_string(self__38)
    retv68 = t69
    return retv68
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env58 closure_env_f4_0, w__7 int32) int32 {
    var retv71 int32
    var a__0 int32 = env58.a_0
    var b__2 int32 = env58.b_1
    var c__4 int32 = env58.c_2
    var d__6 int32 = env58.d_3
    var x__1 int32 = env58.x_4
    var y__3 int32 = env58.y_5
    var z__5 int32 = env58.z_6
    var t72 int32 = a__0 + b__2
    var t73 int32 = t72 + c__4
    var t74 int32 = t73 + d__6
    var t75 int32 = t74 + x__1
    var t76 int32 = t75 + y__3
    var t77 int32 = t76 + z__5
    var t78 int32 = t77 + w__7
    retv71 = t78
    return retv71
}

func _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(env59 closure_env_f3_1, z__5 int32) int32 {
    var retv80 int32
    var a__0 int32 = env59.a_0
    var b__2 int32 = env59.b_1
    var c__4 int32 = env59.c_2
    var x__1 int32 = env59.x_3
    var y__3 int32 = env59.y_4
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
    var t81 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(f4__8, 4)
    retv80 = t81
    return retv80
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env60 closure_env_f2_2, y__3 int32) int32 {
    var retv83 int32
    var a__0 int32 = env60.a_0
    var b__2 int32 = env60.b_1
    var x__1 int32 = env60.x_2
    var c__4 int32 = 30
    var f3__9 closure_env_f3_1 = closure_env_f3_1{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        x_3: x__1,
        y_4: y__3,
    }
    var t84 int32 = _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(f3__9, 3)
    retv83 = t84
    return retv83
}

func _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(env61 closure_env_f1_3, x__1 int32) int32 {
    var retv86 int32
    var a__0 int32 = env61.a_0
    var b__2 int32 = 20
    var f2__10 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    var t87 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(f2__10, 2)
    retv86 = t87
    return retv86
}

func main() {
    main0()
}
