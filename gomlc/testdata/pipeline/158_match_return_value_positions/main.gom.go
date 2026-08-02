package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_f_0 struct {}

func add_after_match(flag__0 bool) int32 {
    var jp165 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp165 = 7
        var t166 int32 = jp165 + 1
        return t166
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var jp170 int
    switch flag__2 {
    case true:
        return "early"
    case false:
        jp170 = 7
        var t171 string = _goml_m_inherent_i_int_i_int_i_to__string(jp170)
        return t171
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t174 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    return t174
}

func main0() struct{} {
    var t176 int32 = add_after_match(false)
    println__T_int32(t176)
    var t177 int32 = add_after_match(true)
    println__T_int32(t177)
    var t178 string = receiver_after_match(false)
    println__T_string(t178)
    var t179 string = receiver_after_match(true)
    println__T_string(t179)
    var t180 int32 = closure_after_match(false)
    println__T_int32(t180)
    var t181 int32 = closure_after_match(true)
    println__T_int32(t181)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t184 string = _goml_runtime_core_int_to_string(self__5)
    return t184
}

func println__T_int32(value__1 int32) struct{} {
    var t186 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t193 string = _goml_runtime_core_int32_to_string(self__43)
    return t193
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env161 closure_env_f_0, inner__4 bool) int32 {
    var jp199 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp199 = 4
        var t200 int32 = jp199 + 3
        return t200
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
