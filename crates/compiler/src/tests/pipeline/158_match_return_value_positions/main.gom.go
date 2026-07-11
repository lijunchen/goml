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
    var retv15 int32
    var jp17 int32
    switch flag__0 {
    case true:
        retv15 = 5
        return retv15
    case false:
        jp17 = 7
        var value__1 int32 = jp17
        var t18 int32 = value__1 + 1
        retv15 = t18
        return retv15
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv20 string
    var jp22 int32
    switch flag__2 {
    case true:
        retv20 = "early"
        return retv20
    case false:
        jp22 = 7
        var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(jp22)
        retv20 = t23
        return retv20
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv25 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t26 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv25 = t26
    return retv25
}

func main0() struct{} {
    var t28 int32 = add_after_match(false)
    println__T_int32(t28)
    var t29 int32 = add_after_match(true)
    println__T_int32(t29)
    var t30 string = receiver_after_match(false)
    println__T_string(t30)
    var t31 string = receiver_after_match(true)
    println__T_string(t31)
    var t32 int32 = closure_after_match(false)
    println__T_int32(t32)
    var t33 int32 = closure_after_match(true)
    println__T_int32(t33)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv35 string
    var t36 string = _goml_runtime_core_int32_to_string(self__2)
    retv35 = t36
    return retv35
}

func println__T_int32(value__1 int32) struct{} {
    var t38 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t38)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t41 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t41)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv44 string
    var t45 string = _goml_runtime_core_int32_to_string(self__13)
    retv44 = t45
    return retv44
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv47 string
    retv47 = self__9
    return retv47
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env13 closure_env_f_0, inner__4 bool) int32 {
    var retv49 int32
    var jp51 int32
    switch inner__4 {
    case true:
        retv49 = 2
        return retv49
    case false:
        jp51 = 4
        var value__5 int32 = jp51
        var t52 int32 = value__5 + 3
        retv49 = t52
        return retv49
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
