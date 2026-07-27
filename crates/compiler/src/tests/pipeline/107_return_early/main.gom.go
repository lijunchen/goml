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
    var retv83 int32
    var t88 bool = x__0 < 0
    if t88 {
        retv83 = 0
        return retv83
    } else {
        var t87 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(x__0, 0)
        if t87 {
            retv83 = 1
            return retv83
        } else {
            var t86 int32 = x__0 + 2
            retv83 = t86
            return retv83
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv90 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t91 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv90 = t91
    return retv90
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
    var t95 int32 = early(-1)
    println__T_int32(t95)
    print__T_string("e0: ")
    var t96 int32 = early(0)
    println__T_int32(t96)
    print__T_string("e3: ")
    var t97 int32 = early(3)
    println__T_int32(t97)
    print__T_string("c7: ")
    var t98 int32 = closure_early(7)
    println__T_int32(t98)
    print__T_string("c2: ")
    var t99 int32 = closure_early(2)
    println__T_int32(t99)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv101 bool
    var t102 bool = self__65 == other__66
    retv101 = t102
    return retv101
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t107 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t107)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t110 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t110)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv113 string
    retv113 = self__38
    return retv113
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv115 string
    var t116 string = _goml_runtime_core_int32_to_string(self__43)
    retv115 = t116
    return retv115
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env81 closure_env_f_0, y__2 int32) int32 {
    var retv118 int32
    var t121 bool = y__2 > 5
    if t121 {
        retv118 = y__2
        return retv118
    } else {
        var t120 int32 = y__2 + 10
        retv118 = t120
        return retv118
    }
}

func main() {
    main0()
}
