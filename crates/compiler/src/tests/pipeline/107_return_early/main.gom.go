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
    var retv77 int32
    var t82 bool = x__0 < 0
    if t82 {
        retv77 = 0
        return retv77
    } else {
        var t81 bool = x__0 == 0
        if t81 {
            retv77 = 1
            return retv77
        } else {
            var t80 int32 = x__0 + 2
            retv77 = t80
            return retv77
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv84 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t85 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv84 = t85
    return retv84
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
    var t89 int32 = -1
    var t90 int32 = early(t89)
    println__T_int32(t90)
    print__T_string("e0: ")
    var t91 int32 = early(0)
    println__T_int32(t91)
    print__T_string("e3: ")
    var t92 int32 = early(3)
    println__T_int32(t92)
    print__T_string("c7: ")
    var t93 int32 = closure_early(7)
    println__T_int32(t93)
    print__T_string("c2: ")
    var t94 int32 = closure_early(2)
    println__T_int32(t94)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t99)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv105 string
    retv105 = self__34
    return retv105
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv107 string
    var t108 string = _goml_runtime_core_int32_to_string(self__38)
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env75 closure_env_f_0, y__2 int32) int32 {
    var retv110 int32
    var t113 bool = y__2 > 5
    if t113 {
        retv110 = y__2
        return retv110
    } else {
        var t112 int32 = y__2 + 10
        retv110 = t112
        return retv110
    }
}

func main() {
    main0()
}
