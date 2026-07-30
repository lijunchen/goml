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
    var retv127 int32
    var t132 bool = x__0 < 0
    if t132 {
        retv127 = 0
        return retv127
    } else {
        var t131 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(x__0, 0)
        if t131 {
            retv127 = 1
            return retv127
        } else {
            var t130 int32 = x__0 + 2
            retv127 = t130
            return retv127
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv134 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t135 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv134 = t135
    return retv134
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
    var t139 int32 = early(-1)
    println__T_int32(t139)
    print__T_string("e0: ")
    var t140 int32 = early(0)
    println__T_int32(t140)
    print__T_string("e3: ")
    var t141 int32 = early(3)
    println__T_int32(t141)
    print__T_string("c7: ")
    var t142 int32 = closure_early(7)
    println__T_int32(t142)
    print__T_string("c2: ")
    var t143 int32 = closure_early(2)
    println__T_int32(t143)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv145 bool
    var t146 bool = self__65 == other__66
    retv145 = t146
    return retv145
}

func println__T_string(value__1 string) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t151 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t151)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t154 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t154)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv157 string
    retv157 = self__38
    return retv157
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv159 string
    var t160 string = _goml_runtime_core_int32_to_string(self__43)
    retv159 = t160
    return retv159
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env125 closure_env_f_0, y__2 int32) int32 {
    var retv162 int32
    var t165 bool = y__2 > 5
    if t165 {
        retv162 = y__2
        return retv162
    } else {
        var t164 int32 = y__2 + 10
        retv162 = t164
        return retv162
    }
}

func main() {
    main0()
}
