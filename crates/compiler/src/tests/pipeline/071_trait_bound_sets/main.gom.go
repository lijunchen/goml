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
    var retv61 string
    retv61 = "A"
    return retv61
}

func _goml_m_trait__impl_i_C_i_S_i_bar(self__2 S) string {
    var retv65 string
    retv65 = "C"
    return retv65
}

func main0() struct{} {
    var s__5 S = S{}
    var t67 string = pick_a__T_S(s__5)
    println__T_string(t67)
    var t68 string = bar_it__T_S(s__5)
    println__T_string(t68)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t70)
    return struct{}{}
}

func pick_a__T_S(x__3 S) string {
    var retv73 string
    var t74 string = _goml_m_trait__impl_i_A_i_S_i_foo(x__3)
    retv73 = t74
    return retv73
}

func bar_it__T_S(x__4 S) string {
    var retv76 string
    var t77 string = _goml_m_trait__impl_i_C_i_S_i_bar(x__4)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv79 string
    retv79 = self__34
    return retv79
}

func main() {
    main0()
}
