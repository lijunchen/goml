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
    var retv76 int32
    var jp78 int32
    switch flag__0 {
    case true:
        retv76 = 5
        return retv76
    case false:
        jp78 = 7
        var value__1 int32 = jp78
        var t79 int32 = value__1 + 1
        retv76 = t79
        return retv76
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv81 string
    var jp83 int
    switch flag__2 {
    case true:
        retv81 = "early"
        return retv81
    case false:
        jp83 = 7
        var t84 string = _goml_m_inherent_i_int_i_int_i_to__string(jp83)
        retv81 = t84
        return retv81
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv86 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t87 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv86 = t87
    return retv86
}

func main0() struct{} {
    var t89 int32 = add_after_match(false)
    println__T_int32(t89)
    var t90 int32 = add_after_match(true)
    println__T_int32(t90)
    var t91 string = receiver_after_match(false)
    println__T_string(t91)
    var t92 string = receiver_after_match(true)
    println__T_string(t92)
    var t93 int32 = closure_after_match(false)
    println__T_int32(t93)
    var t94 int32 = closure_after_match(true)
    println__T_int32(t94)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv96 string
    var t97 string = _goml_runtime_core_int_to_string(self__5)
    retv96 = t97
    return retv96
}

func println__T_int32(value__1 int32) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv105 string
    var t106 string = _goml_runtime_core_int32_to_string(self__43)
    retv105 = t106
    return retv105
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv108 string
    retv108 = self__38
    return retv108
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env74 closure_env_f_0, inner__4 bool) int32 {
    var retv110 int32
    var jp112 int32
    switch inner__4 {
    case true:
        retv110 = 2
        return retv110
    case false:
        jp112 = 4
        var value__5 int32 = jp112
        var t113 int32 = value__5 + 3
        retv110 = t113
        return retv110
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
