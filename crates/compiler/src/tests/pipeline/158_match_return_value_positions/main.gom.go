package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_f_0 struct {}

func add_after_match(flag__0 bool) int32 {
    var retv12 int32
    var jp14 int32
    switch flag__0 {
    case true:
        retv12 = 5
        return retv12
    case false:
        jp14 = 7
        var value__1 int32 = jp14
        var t15 int32 = value__1 + 1
        retv12 = t15
        return retv12
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv17 string
    var jp19 int32
    switch flag__2 {
    case true:
        retv17 = "early"
        return retv17
    case false:
        jp19 = 7
        var t20 string = _goml_m_inherent_i_int32_i_int32_i_to__string(jp19)
        retv17 = t20
        return retv17
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv22 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t23 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv22 = t23
    return retv22
}

func main0() struct{} {
    var t25 int32 = add_after_match(false)
    println__T_int32(t25)
    var t26 int32 = add_after_match(true)
    println__T_int32(t26)
    var t27 string = receiver_after_match(false)
    println__T_string(t27)
    var t28 string = receiver_after_match(true)
    println__T_string(t28)
    var t29 int32 = closure_after_match(false)
    println__T_int32(t29)
    var t30 int32 = closure_after_match(true)
    println__T_int32(t30)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv32 string
    var t33 string = _goml_runtime_core_int32_to_string(self__2)
    retv32 = t33
    return retv32
}

func println__T_int32(value__1 int32) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t38 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t38)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv41 string
    var t42 string = _goml_runtime_core_int32_to_string(self__13)
    retv41 = t42
    return retv41
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv44 string
    retv44 = self__9
    return retv44
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env10 closure_env_f_0, inner__4 bool) int32 {
    var retv46 int32
    var jp48 int32
    switch inner__4 {
    case true:
        retv46 = 2
        return retv46
    case false:
        jp48 = 4
        var value__5 int32 = jp48
        var t49 int32 = value__5 + 3
        retv46 = t49
        return retv46
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
