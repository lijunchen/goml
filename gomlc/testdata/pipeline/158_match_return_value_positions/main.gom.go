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
    var retv160 int32
    var jp162 int32
    switch flag__0 {
    case true:
        retv160 = 5
        return retv160
    case false:
        jp162 = 7
        var value__1 int32 = jp162
        var t163 int32 = value__1 + 1
        retv160 = t163
        return retv160
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv165 string
    var jp167 int
    switch flag__2 {
    case true:
        retv165 = "early"
        return retv165
    case false:
        jp167 = 7
        var t168 string = _goml_m_inherent_i_int_i_int_i_to__string(jp167)
        retv165 = t168
        return retv165
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv170 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t171 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv170 = t171
    return retv170
}

func main0() struct{} {
    var t173 int32 = add_after_match(false)
    println__T_int32(t173)
    var t174 int32 = add_after_match(true)
    println__T_int32(t174)
    var t175 string = receiver_after_match(false)
    println__T_string(t175)
    var t176 string = receiver_after_match(true)
    println__T_string(t176)
    var t177 int32 = closure_after_match(false)
    println__T_int32(t177)
    var t178 int32 = closure_after_match(true)
    println__T_int32(t178)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv180 string
    var t181 string = _goml_runtime_core_int_to_string(self__5)
    retv180 = t181
    return retv180
}

func println__T_int32(value__1 int32) struct{} {
    var t183 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t183)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t186)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv189 string
    var t190 string = _goml_runtime_core_int32_to_string(self__43)
    retv189 = t190
    return retv189
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv192 string
    retv192 = self__38
    return retv192
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env158 closure_env_f_0, inner__4 bool) int32 {
    var retv194 int32
    var jp196 int32
    switch inner__4 {
    case true:
        retv194 = 2
        return retv194
    case false:
        jp196 = 4
        var value__5 int32 = jp196
        var t197 int32 = value__5 + 3
        retv194 = t197
        return retv194
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
