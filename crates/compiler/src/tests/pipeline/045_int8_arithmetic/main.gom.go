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
    var retv70 int8
    retv70 = 90
    return retv70
}

func b_value() int8 {
    var retv72 int8
    var t73 int8 = -20
    retv72 = t73
    return retv72
}

func c_value() int8 {
    var retv75 int8
    retv75 = 3
    return retv75
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    var t78 string = label__0 + t77
    println__T_string(t78)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t81 string = label__2 + t80
    println__T_string(t81)
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
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__36 int8) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int8_to_string(self__36)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv90 string
    var t91 string = _goml_runtime_core_bool_to_string(self__33)
    retv90 = t91
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv93 string
    retv93 = self__34
    return retv93
}

func main() {
    main0()
}
