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

type S struct {}

func _goml_m_trait__impl_i_A_i_S_i_pick(self__0 S) int32 {
    var retv71 int32
    retv71 = 10
    return retv71
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    var retv73 int32
    retv73 = 20
    return retv73
}

func main0() struct{} {
    var t75 S = S{}
    var t76 int32 = pick_a__T_S(t75)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    println__T_string(t77)
    var t78 S = S{}
    var t79 int32 = pick_b__T_S(t78)
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t79)
    println__T_string(t80)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func pick_a__T_S(x__2 S) int32 {
    var retv85 int32
    var t86 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(x__2)
    retv85 = t86
    return retv85
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv88 string
    var t89 string = _goml_runtime_core_int32_to_string(self__6)
    retv88 = t89
    return retv88
}

func pick_b__T_S(x__3 S) int32 {
    var retv91 int32
    var t92 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(x__3)
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv94 string
    retv94 = self__38
    return retv94
}

func main() {
    main0()
}
