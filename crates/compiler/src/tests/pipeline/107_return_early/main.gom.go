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
    var retv41 int32
    var t46 bool = x__0 < 0
    if t46 {
        retv41 = 0
        return retv41
    } else {
        var t45 bool = x__0 == 0
        if t45 {
            retv41 = 1
            return retv41
        } else {
            var t44 int32 = x__0 + 2
            retv41 = t44
            return retv41
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv48 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t49 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv48 = t49
    return retv48
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
    var t53 int32 = -1
    var t54 int32 = early(t53)
    println__T_int32(t54)
    print__T_string("e0: ")
    var t55 int32 = early(0)
    println__T_int32(t55)
    print__T_string("e3: ")
    var t56 int32 = early(3)
    println__T_int32(t56)
    print__T_string("c7: ")
    var t57 int32 = closure_early(7)
    println__T_int32(t57)
    print__T_string("c2: ")
    var t58 int32 = closure_early(2)
    println__T_int32(t58)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t60 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t60)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t63 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t63)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t66 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t66)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv69 string
    retv69 = self__9
    return retv69
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv71 string
    var t72 string = _goml_runtime_core_int32_to_string(self__13)
    retv71 = t72
    return retv71
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env39 closure_env_f_0, y__2 int32) int32 {
    var retv74 int32
    var t77 bool = y__2 > 5
    if t77 {
        retv74 = y__2
        return retv74
    } else {
        var t76 int32 = y__2 + 10
        retv74 = t76
        return retv74
    }
}

func main() {
    main0()
}
