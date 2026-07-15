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
    var retv34 int8
    retv34 = 90
    return retv34
}

func b_value() int8 {
    var retv36 int8
    var t37 int8 = -20
    retv36 = t37
    return retv36
}

func c_value() int8 {
    var retv39 int8
    retv39 = 3
    return retv39
}

func show_int8(label__0 string, value__1 int8) struct{} {
    var t41 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    var t42 string = label__0 + t41
    println__T_string(t42)
    return struct{}{}
}

func show_bool(label__2 string, value__3 bool) struct{} {
    var t44 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__3)
    var t45 string = label__2 + t44
    println__T_string(t45)
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
    var t48 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t48)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__11 int8) string {
    var retv51 string
    var t52 string = _goml_runtime_core_int8_to_string(self__11)
    retv51 = t52
    return retv51
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv54 string
    var t55 string = _goml_runtime_core_bool_to_string(self__8)
    retv54 = t55
    return retv54
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv57 string
    retv57 = self__9
    return retv57
}

func main() {
    main0()
}
