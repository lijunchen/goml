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
    var retv66 int32
    var jp68 int32
    switch flag__0 {
    case true:
        retv66 = 5
        return retv66
    case false:
        jp68 = 7
        var value__1 int32 = jp68
        var t69 int32 = value__1 + 1
        retv66 = t69
        return retv66
    default:
        panic("non-exhaustive match")
    }
}

func receiver_after_match(flag__2 bool) string {
    var retv71 string
    var jp73 int32
    switch flag__2 {
    case true:
        retv71 = "early"
        return retv71
    case false:
        jp73 = 7
        var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(jp73)
        retv71 = t74
        return retv71
    default:
        panic("non-exhaustive match")
    }
}

func closure_after_match(flag__3 bool) int32 {
    var retv76 int32
    var f__6 closure_env_f_0 = closure_env_f_0{}
    var t77 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__6, flag__3)
    retv76 = t77
    return retv76
}

func main0() struct{} {
    var t79 int32 = add_after_match(false)
    println__T_int32(t79)
    var t80 int32 = add_after_match(true)
    println__T_int32(t80)
    var t81 string = receiver_after_match(false)
    println__T_string(t81)
    var t82 string = receiver_after_match(true)
    println__T_string(t82)
    var t83 int32 = closure_after_match(false)
    println__T_int32(t83)
    var t84 int32 = closure_after_match(true)
    println__T_int32(t84)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv86 string
    var t87 string = _goml_runtime_core_int32_to_string(self__2)
    retv86 = t87
    return retv86
}

func println__T_int32(value__1 int32) struct{} {
    var t89 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t89)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv95 string
    var t96 string = _goml_runtime_core_int32_to_string(self__38)
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv98 string
    retv98 = self__34
    return retv98
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env64 closure_env_f_0, inner__4 bool) int32 {
    var retv100 int32
    var jp102 int32
    switch inner__4 {
    case true:
        retv100 = 2
        return retv100
    case false:
        jp102 = 4
        var value__5 int32 = jp102
        var t103 int32 = value__5 + 3
        retv100 = t103
        return retv100
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
