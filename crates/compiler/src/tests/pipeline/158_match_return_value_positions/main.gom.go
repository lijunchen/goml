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
    var retv72 int32
    var jp74 int32
    switch flag__0 {
    case true:
        retv72 = 5
        return retv72
    case false:
        jp74 = 7
        var value__1 int32 = jp74
        var t75 int32 = value__1 + 1
        retv72 = t75
        return retv72
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv77 string
    var jp79 int
    switch flag__2 {
    case true:
        retv77 = "early"
        return retv77
    case false:
        jp79 = 7
        var t80 string = _goml_m_inherent_i_int_i_int_i_to__string(jp79)
        retv77 = t80
        return retv77
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv82 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t83 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv82 = t83
    return retv82
}

func main0() struct{} {
    var t85 int32 = add_after_match(false)
    println__T_int32(t85)
    var t86 int32 = add_after_match(true)
    println__T_int32(t86)
    var t87 string = receiver_after_match(false)
    println__T_string(t87)
    var t88 string = receiver_after_match(true)
    println__T_string(t88)
    var t89 int32 = closure_after_match(false)
    println__T_int32(t89)
    var t90 int32 = closure_after_match(true)
    println__T_int32(t90)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv92 string
    var t93 string = _goml_runtime_core_int_to_string(self__5)
    retv92 = t93
    return retv92
}

func println__T_int32(value__1 int32) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int32_to_string(self__43)
    retv101 = t102
    return retv101
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv104 string
    retv104 = self__38
    return retv104
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env70 closure_env_f_0, inner__4 bool) int32 {
    var retv106 int32
    var jp108 int32
    switch inner__4 {
    case true:
        retv106 = 2
        return retv106
    case false:
        jp108 = 4
        var value__5 int32 = jp108
        var t109 int32 = value__5 + 3
        retv106 = t109
        return retv106
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
