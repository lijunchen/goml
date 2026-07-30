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
    var retv87 int32
    var t92 bool = x__0 < 0
    if t92 {
        retv87 = 0
        return retv87
    } else {
        var t91 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(x__0, 0)
        if t91 {
            retv87 = 1
            return retv87
        } else {
            var t90 int32 = x__0 + 2
            retv87 = t90
            return retv87
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv94 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t95 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv94 = t95
    return retv94
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
    var t99 int32 = early(-1)
    println__T_int32(t99)
    print__T_string("e0: ")
    var t100 int32 = early(0)
    println__T_int32(t100)
    print__T_string("e3: ")
    var t101 int32 = early(3)
    println__T_int32(t101)
    print__T_string("c7: ")
    var t102 int32 = closure_early(7)
    println__T_int32(t102)
    print__T_string("c2: ")
    var t103 int32 = closure_early(2)
    println__T_int32(t103)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv105 bool
    var t106 bool = self__65 == other__66
    retv105 = t106
    return retv105
}

func println__T_string(value__1 string) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t111 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t111)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t114)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv117 string
    retv117 = self__38
    return retv117
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv119 string
    var t120 string = _goml_runtime_core_int32_to_string(self__43)
    retv119 = t120
    return retv119
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env85 closure_env_f_0, y__2 int32) int32 {
    var retv122 int32
    var t125 bool = y__2 > 5
    if t125 {
        retv122 = y__2
        return retv122
    } else {
        var t124 int32 = y__2 + 10
        retv122 = t124
        return retv122
    }
}

func main() {
    main0()
}
