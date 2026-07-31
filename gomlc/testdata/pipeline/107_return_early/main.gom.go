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
    var retv171 int32
    var t176 bool = x__0 < 0
    if t176 {
        retv171 = 0
        return retv171
    } else {
        var t175 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(x__0, 0)
        if t175 {
            retv171 = 1
            return retv171
        } else {
            var t174 int32 = x__0 + 2
            retv171 = t174
            return retv171
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv178 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t179 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv178 = t179
    return retv178
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
    var t183 int32 = early(-1)
    println__T_int32(t183)
    print__T_string("e0: ")
    var t184 int32 = early(0)
    println__T_int32(t184)
    print__T_string("e3: ")
    var t185 int32 = early(3)
    println__T_int32(t185)
    print__T_string("c7: ")
    var t186 int32 = closure_early(7)
    println__T_int32(t186)
    print__T_string("c2: ")
    var t187 int32 = closure_early(2)
    println__T_int32(t187)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv189 bool
    var t190 bool = self__65 == other__66
    retv189 = t190
    return retv189
}

func println__T_string(value__1 string) struct{} {
    var t192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t192)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t195)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t198 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv201 string
    retv201 = self__38
    return retv201
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv203 string
    var t204 string = _goml_runtime_core_int32_to_string(self__43)
    retv203 = t204
    return retv203
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env169 closure_env_f_0, y__2 int32) int32 {
    var retv206 int32
    var t209 bool = y__2 > 5
    if t209 {
        retv206 = y__2
        return retv206
    } else {
        var t208 int32 = y__2 + 10
        retv206 = t208
        return retv206
    }
}

func main() {
    main0()
}
