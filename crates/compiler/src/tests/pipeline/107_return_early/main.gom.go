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
    var retv26 int32
    var t31 bool = x__0 < 0
    if t31 {
        retv26 = 0
        return retv26
    } else {
        var t30 bool = x__0 == 0
        if t30 {
            retv26 = 1
            return retv26
        } else {
            var t29 int32 = x__0 + 2
            retv26 = t29
            return retv26
        }
    }
}

func closure_early(x__1 int32) int32 {
    var retv33 int32
    var f__3 closure_env_f_0 = closure_env_f_0{}
    var t34 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__3, x__1)
    retv33 = t34
    return retv33
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
    var t38 int32 = -1
    var t39 int32 = early(t38)
    println__T_int32(t39)
    print__T_string("e0: ")
    var t40 int32 = early(0)
    println__T_int32(t40)
    print__T_string("e3: ")
    var t41 int32 = early(3)
    println__T_int32(t41)
    print__T_string("c7: ")
    var t42 int32 = closure_early(7)
    println__T_int32(t42)
    print__T_string("c2: ")
    var t43 int32 = closure_early(2)
    println__T_int32(t43)
    unit_ret(true)
    unit_ret(false)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t45 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t45)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t48 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t48)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t51)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv56 string
    var t57 string = _goml_runtime_core_int32_to_string(self__13)
    retv56 = t57
    return retv56
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env24 closure_env_f_0, y__2 int32) int32 {
    var retv59 int32
    var t62 bool = y__2 > 5
    if t62 {
        retv59 = y__2
        return retv59
    } else {
        var t61 int32 = y__2 + 10
        retv59 = t61
        return retv59
    }
}

func main() {
    main0()
}
