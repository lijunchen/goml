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

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_m_trait__impl_i_Arith_i_int32_i_add(self__0 int32, other__1 int32) int32 {
    var retv63 int32
    var t64 int32 = self__0 + other__1
    retv63 = t64
    return retv63
}

func _goml_m_trait__impl_i_Arith_i_int32_i_less(self__2 int32, other__3 int32) bool {
    var retv66 bool
    var t67 bool = self__2 < other__3
    retv66 = t67
    return retv66
}

func _goml_m_trait__impl_i_Output_i_int32_i_output(self__4 int32) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__4)
    println__T_string(t69)
    return struct{}{}
}

func _goml_m_trait__impl_i_Output_i_bool_i_output(self__5 bool) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__5)
    println__T_string(t72)
    return struct{}{}
}

func main0() struct{} {
    var a__7 int32 = id__T_int32(1)
    var b__8 int32 = id__T_int32(2)
    var c__9 int32 = _goml_m_trait__impl_i_Arith_i_int32_i_add(a__7, b__8)
    _goml_m_trait__impl_i_Output_i_int32_i_output(c__9)
    var a__10 int32 = id__T_int32(3)
    var b__11 int32 = id__T_int32(4)
    var c__12 bool = _goml_m_trait__impl_i_Arith_i_int32_i_less(a__10, b__11)
    _goml_m_trait__impl_i_Output_i_bool_i_output(c__12)
    id__T_string("abc")
    id__T_bool(true)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__38)
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv82 string
    var t83 string = _goml_runtime_core_bool_to_string(self__33)
    retv82 = t83
    return retv82
}

func id__T_int32(x__6 int32) int32 {
    var retv85 int32
    retv85 = x__6
    return retv85
}

func id__T_string(x__6 string) string {
    var retv87 string
    retv87 = x__6
    return retv87
}

func id__T_bool(x__6 bool) bool {
    var retv89 bool
    retv89 = x__6
    return retv89
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv91 string
    retv91 = self__34
    return retv91
}

func main() {
    main0()
}
