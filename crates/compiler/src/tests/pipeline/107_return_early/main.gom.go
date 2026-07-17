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
    var retv80 int32
    var t85 bool = x__0 < 0
    if t85 {
        retv80 = 0
        return retv80
    } else {
        var t84 bool = x__0 == 0
        if t84 {
            retv80 = 1
            return retv80
        } else {
            var t83 int32 = x__0 + 2
            retv80 = t83
            return retv80
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv87 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t88 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv87 = t88
    return retv87
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
    var t92 int32 = -1
    var t93 int32 = early(t92)
    println__T_int32(t93)
    print__T_string("e0: ")
    var t94 int32 = early(0)
    println__T_int32(t94)
    print__T_string("e3: ")
    var t95 int32 = early(3)
    println__T_int32(t95)
    print__T_string("c7: ")
    var t96 int32 = closure_early(7)
    println__T_int32(t96)
    print__T_string("c2: ")
    var t97 int32 = closure_early(2)
    println__T_int32(t97)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t102)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t105)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv108 string
    retv108 = self__37
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv110 string
    var t111 string = _goml_runtime_core_int32_to_string(self__41)
    retv110 = t111
    return retv110
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env78 closure_env_f_0, y__2 int32) int32 {
    var retv113 int32
    var t116 bool = y__2 > 5
    if t116 {
        retv113 = y__2
        return retv113
    } else {
        var t115 int32 = y__2 + 10
        retv113 = t115
        return retv113
    }
}

func main() {
    main0()
}
