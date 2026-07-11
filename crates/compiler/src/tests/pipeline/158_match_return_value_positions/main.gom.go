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
    var retv30 int32
    var jp32 int32
    switch flag__0 {
    case true:
        retv30 = 5
        return retv30
    case false:
        jp32 = 7
        var value__1 int32 = jp32
        var t33 int32 = value__1 + 1
        retv30 = t33
        return retv30
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv35 string
    var jp37 int32
    switch flag__2 {
    case true:
        retv35 = "early"
        return retv35
    case false:
        jp37 = 7
        var t38 string = _goml_m_inherent_i_int32_i_int32_i_to__string(jp37)
        retv35 = t38
        return retv35
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv40 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t41 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv40 = t41
    return retv40
}

func main0() struct{} {
    var t43 int32 = add_after_match(false)
    println__T_int32(t43)
    var t44 int32 = add_after_match(true)
    println__T_int32(t44)
    var t45 string = receiver_after_match(false)
    println__T_string(t45)
    var t46 string = receiver_after_match(true)
    println__T_string(t46)
    var t47 int32 = closure_after_match(false)
    println__T_int32(t47)
    var t48 int32 = closure_after_match(true)
    println__T_int32(t48)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv50 string
    var t51 string = _goml_runtime_core_int32_to_string(self__2)
    retv50 = t51
    return retv50
}

func println__T_int32(value__1 int32) struct{} {
    var t53 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t53)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t56 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t56)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv59 string
    var t60 string = _goml_runtime_core_int32_to_string(self__13)
    retv59 = t60
    return retv59
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv62 string
    retv62 = self__9
    return retv62
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env28 closure_env_f_0, inner__4 bool) int32 {
    var retv64 int32
    var jp66 int32
    switch inner__4 {
    case true:
        retv64 = 2
        return retv64
    case false:
        jp66 = 4
        var value__5 int32 = jp66
        var t67 int32 = value__5 + 3
        retv64 = t67
        return retv64
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
