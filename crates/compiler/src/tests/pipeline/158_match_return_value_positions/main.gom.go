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
    var retv69 int32
    var jp71 int32
    switch flag__0 {
    case true:
        retv69 = 5
        return retv69
    case false:
        jp71 = 7
        var value__1 int32 = jp71
        var t72 int32 = value__1 + 1
        retv69 = t72
        return retv69
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv74 string
    var jp76 int32
    switch flag__2 {
    case true:
        retv74 = "early"
        return retv74
    case false:
        jp76 = 7
        var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(jp76)
        retv74 = t77
        return retv74
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv79 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t80 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv79 = t80
    return retv79
}

func main0() struct{} {
    var t82 int32 = add_after_match(false)
    println__T_int32(t82)
    var t83 int32 = add_after_match(true)
    println__T_int32(t83)
    var t84 string = receiver_after_match(false)
    println__T_string(t84)
    var t85 string = receiver_after_match(true)
    println__T_string(t85)
    var t86 int32 = closure_after_match(false)
    println__T_int32(t86)
    var t87 int32 = closure_after_match(true)
    println__T_int32(t87)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv89 string
    var t90 string = _goml_runtime_core_int32_to_string(self__5)
    retv89 = t90
    return retv89
}

func println__T_int32(value__1 int32) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv98 string
    var t99 string = _goml_runtime_core_int32_to_string(self__41)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv101 string
    retv101 = self__37
    return retv101
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env67 closure_env_f_0, inner__4 bool) int32 {
    var retv103 int32
    var jp105 int32
    switch inner__4 {
    case true:
        retv103 = 2
        return retv103
    case false:
        jp105 = 4
        var value__5 int32 = jp105
        var t106 int32 = value__5 + 3
        retv103 = t106
        return retv103
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
