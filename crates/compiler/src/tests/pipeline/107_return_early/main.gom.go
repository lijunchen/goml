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
    var retv23 int32
    var t28 bool = x__0 < 0
    if t28 {
        retv23 = 0
        return retv23
    } else {
        var t27 bool = x__0 == 0
        if t27 {
            retv23 = 1
            return retv23
        } else {
            var t26 int32 = x__0 + 2
            retv23 = t26
            return retv23
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv30 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t31 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv30 = t31
    return retv30
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
    var t35 int32 = -1
    var t36 int32 = early(t35)
    println__T_int32(t36)
    print__T_string("e0: ")
    var t37 int32 = early(0)
    println__T_int32(t37)
    print__T_string("e3: ")
    var t38 int32 = early(3)
    println__T_int32(t38)
    print__T_string("c7: ")
    var t39 int32 = closure_early(7)
    println__T_int32(t39)
    print__T_string("c2: ")
    var t40 int32 = closure_early(2)
    println__T_int32(t40)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t42 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t42)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t45 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t45)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t48 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t48)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv51 string
    retv51 = self__9
    return retv51
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv53 string
    var t54 string = _goml_runtime_core_int32_to_string(self__13)
    retv53 = t54
    return retv53
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env21 closure_env_f_0, y__2 int32) int32 {
    var retv56 int32
    var t59 bool = y__2 > 5
    if t59 {
        retv56 = y__2
        return retv56
    } else {
        var t58 int32 = y__2 + 10
        retv56 = t58
        return retv56
    }
}

func main() {
    main0()
}
