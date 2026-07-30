package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func a_value() int8 {
    var retv120 int8
    retv120 = 90
    return retv120
}

func b_value() int8 {
    var retv122 int8
    retv122 = -20
    return retv122
}

func c_value() int8 {
    var retv124 int8
    retv124 = 3
    return retv124
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    var t127 string = label__0 + t126
    println__T_string(t127)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t129 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t130 string = label__2 + t129
    println__T_string(t130)
    return struct{}{}
}

func main0() struct{} {
    var a__4 int8 = a_value()
    var b__5 int8 = b_value()
    var c__6 int8 = c_value()
    var sum__7 int8 = a__4 + b__5
    var diff__8 int8 = a__4 - c__6
    var prod__9 int8 = b__5 * c__6
    var quot__10 int8 = a__4 / c__6
    var neg__11 int8 = -b__5
    var less__12 bool = b__5 < a__4
    show_int8("a=", a__4)
    show_int8("b=", b__5)
    show_int8("c=", c__6)
    show_int8("sum=", sum__7)
    show_int8("diff=", diff__8)
    show_int8("prod=", prod__9)
    show_int8("quot=", quot__10)
    show_int8("neg=", neg__11)
    show_bool("b<a=", less__12)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t133 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t133)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv136 string
    var t137 string = _goml_runtime_core_int8_to_string(self__41)
    retv136 = t137
    return retv136
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv139 string
    var t140 string = _goml_runtime_core_bool_to_string(self__37)
    retv139 = t140
    return retv139
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv142 string
    retv142 = self__38
    return retv142
}

func main() {
    main0()
}
