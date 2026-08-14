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
    var t187 closure_env_f1_3 = closure_env_f1_3{
        a_0: a__0,
    }
    var f1__11 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(t187, p0)
    }
    var result__12 int32 = f1__11(1)
    var inline217 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(result__12)
    _goml_runtime_core_string_println(inline217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t194 string = _goml_runtime_core_int32_to_string(self__70)
    return t194
}

func _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(env182 closure_env_f4_0, w__7 int32) int32 {
    var a__0 int32 = env182.a_0
    var b__2 int32 = env182.b_1
    var c__4 int32 = env182.c_2
    var d__6 int32 = env182.d_3
    var x__1 int32 = env182.x_4
    var y__3 int32 = env182.y_5
    var z__5 int32 = env182.z_6
    var t197 int32 = a__0 + b__2
    var t198 int32 = t197 + c__4
    var t199 int32 = t198 + d__6
    var t200 int32 = t199 + x__1
    var t201 int32 = t200 + y__3
    var t202 int32 = t201 + z__5
    var t203 int32 = t202 + w__7
    return t203
}

func _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(env183 closure_env_f3_1, z__5 int32) int32 {
    var a__0 int32 = env183.a_0
    var b__2 int32 = env183.b_1
    var c__4 int32 = env183.c_2
    var x__1 int32 = env183.x_3
    var y__3 int32 = env183.y_4
    var d__6 int32 = 40
    var t206 closure_env_f4_0 = closure_env_f4_0{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        d_3: d__6,
        x_4: x__1,
        y_5: y__3,
        z_6: z__5,
    }
    var f4__8 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(t206, p0)
    }
    var t207 int32 = f4__8(4)
    return t207
}

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env184 closure_env_f2_2, y__3 int32) int32 {
    var a__0 int32 = env184.a_0
    var b__2 int32 = env184.b_1
    var x__1 int32 = env184.x_2
    var c__4 int32 = 30
    var t210 closure_env_f3_1 = closure_env_f3_1{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        x_3: x__1,
        y_4: y__3,
    }
    var f3__9 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f3__1_i_closure__env__f3__1_i_apply(t210, p0)
    }
    var t211 int32 = f3__9(3)
    return t211
}

func _goml_m_inherent_i_closure__env__f1__3_i_closure__env__f1__3_i_apply(env185 closure_env_f1_3, x__1 int32) int32 {
    var a__0 int32 = env185.a_0
    var b__2 int32 = 20
    var t214 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: b__2,
        x_2: x__1,
    }
    var f2__10 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(t214, p0)
    }
    var t215 int32 = f2__10(2)
    return t215
}

func main() {
    main0()
}
