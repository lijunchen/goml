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
    var retv116 int32
    var jp118 int32
    switch flag__0 {
    case true:
        retv116 = 5
        return retv116
    case false:
        jp118 = 7
        var value__1 int32 = jp118
        var t119 int32 = value__1 + 1
        retv116 = t119
        return retv116
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv121 string
    var jp123 int
    switch flag__2 {
    case true:
        retv121 = "early"
        return retv121
    case false:
        jp123 = 7
        var t124 string = _goml_m_inherent_i_int_i_int_i_to__string(jp123)
        retv121 = t124
        return retv121
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv126 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t127 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv126 = t127
    return retv126
}

func main0() struct{} {
    var t129 int32 = add_after_match(false)
    println__T_int32(t129)
    var t130 int32 = add_after_match(true)
    println__T_int32(t130)
    var t131 string = receiver_after_match(false)
    println__T_string(t131)
    var t132 string = receiver_after_match(true)
    println__T_string(t132)
    var t133 int32 = closure_after_match(false)
    println__T_int32(t133)
    var t134 int32 = closure_after_match(true)
    println__T_int32(t134)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv136 string
    var t137 string = _goml_runtime_core_int_to_string(self__5)
    retv136 = t137
    return retv136
}

func println__T_int32(value__1 int32) struct{} {
    var t139 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t139)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t142 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t142)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv145 string
    var t146 string = _goml_runtime_core_int32_to_string(self__43)
    retv145 = t146
    return retv145
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv148 string
    retv148 = self__38
    return retv148
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env114 closure_env_f_0, inner__4 bool) int32 {
    var retv150 int32
    var jp152 int32
    switch inner__4 {
    case true:
        retv150 = 2
        return retv150
    case false:
        jp152 = 4
        var value__5 int32 = jp152
        var t153 int32 = value__5 + 3
        retv150 = t153
        return retv150
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
