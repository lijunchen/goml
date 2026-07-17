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
    var retv64 string
    retv64 = "A"
    return retv64
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv68 string
    retv68 = "C"
    return retv68
}

func main0() struct{} {
    var s__5 S = S{}
    var t70 string = pick_a__T_S(s__5)
    println__T_string(t70)
    var t71 string = bar_it__T_S(s__5)
    println__T_string(t71)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv76 string
    var t77 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv76 = t77
    return retv76
}

func bar_it__T_S(x__4 S) string {
    var retv79 string
    var t80 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv82 string
    retv82 = self__37
    return retv82
}

func main() {
    main0()
}
