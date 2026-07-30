package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type S struct {}

func _goml_m_trait__impl_i_A_i_S_i_foo(self__0 S) string {
    var retv71 string
    retv71 = "A"
    return retv71
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv75 string
    retv75 = "C"
    return retv75
}

func main0() struct{} {
    var s__5 S = S{}
    var t77 string = pick_a__T_S(s__5)
    println__T_string(t77)
    var t78 string = bar_it__T_S(s__5)
    println__T_string(t78)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv83 string
    var t84 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv83 = t84
    return retv83
}

func bar_it__T_S(x__4 S) string {
    var retv86 string
    var t87 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv86 = t87
    return retv86
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv89 string
    retv89 = self__38
    return retv89
}

func main() {
    main0()
}
