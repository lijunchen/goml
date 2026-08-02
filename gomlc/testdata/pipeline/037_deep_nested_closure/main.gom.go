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
    var inline189 int32 = 1
    var inline191 int32 = 20
    var inline192 closure_env_f2_2 = closure_env_f2_2{
        a_0: a__0,
        b_1: inline191,
        x_2: inline189,
    }
    var inline193 int32 = _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(inline192, 2)
    result__12 = inline193
    var inline186 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(result__12)
    _goml_runtime_core_string_println(inline186)
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

func _goml_m_inherent_i_closure__env__f2__2_i_closure__env__f2__2_i_apply(env157 closure_env_f2_2, y__3 int32) int32 {
    var a__0 int32 = env157.a_0
    var b__2 int32 = env157.b_1
    var x__1 int32 = env157.x_2
    var c__4 int32 = 30
    var inline213 int32 = 3
    var inline219 int32 = 40
    var inline220 closure_env_f4_0 = closure_env_f4_0{
        a_0: a__0,
        b_1: b__2,
        c_2: c__4,
        d_3: inline219,
        x_4: x__1,
        y_5: y__3,
        z_6: inline213,
    }
    var inline221 int32 = _goml_m_inherent_i_closure__env__f4__0_i_closure__env__f4__0_i_apply(inline220, 4)
    return inline221
}

func main() {
    main0()
}
