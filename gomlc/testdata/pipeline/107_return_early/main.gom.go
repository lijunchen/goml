package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_f_0 struct {}

func early(x__0 int32) int32 {
    var t179 bool = x__0 < 0
    if t179 {
        return 0
    } else {
        var t178 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(x__0, 0)
        if t178 {
            return 1
        } else {
            var t177 int32 = x__0 + 2
            return t177
        }
    }
}

func closure_early(x__1 int32) int32 {
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t182 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    return t182
}

func unit_ret(flag__4 bool) struct{} {
    if flag__4 {
        return struct{}{}
    } else {
        println__T_string("after")
        return struct{}{}
    }
}

func main0() struct{} {
    print__T_string("e-1: ")
    var t186 int32 = early(-1)
    println__T_int32(t186)
    print__T_string("e0: ")
    var t187 int32 = early(0)
    println__T_int32(t187)
    print__T_string("e3: ")
    var t188 int32 = early(3)
    println__T_int32(t188)
    print__T_string("c7: ")
    var t189 int32 = closure_early(7)
    println__T_int32(t189)
    print__T_string("c2: ")
    var t190 int32 = closure_early(2)
    println__T_int32(t190)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t193 bool = self__65 == other__66
    return t193
}

func println__T_string(value__1 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t198)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t201 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t207 string = _goml_runtime_core_int32_to_string(self__43)
    return t207
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env172 closure_env_f_0, y__2 int32) int32 {
    var t212 bool = y__2 > 5
    if t212 {
        return y__2
    } else {
        var t211 int32 = y__2 + 10
        return t211
    }
}

func main() {
    main0()
}
