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
    var retv66 int32
    var t67 int32 = self__0 + other__1
    retv66 = t67
    return retv66
}

func _goml_m_trait__impl_i_Arith_i_int32_i_less(self__2 int32, other__3 int32) bool {
    var retv69 bool
    var t70 bool = self__2 < other__3
    retv69 = t70
    return retv69
}

func _goml_m_trait__impl_i_Output_i_int32_i_output(self__4 int32) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__4)
    println__T_string(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_Output_i_bool_i_output(self__5 bool) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__5)
    println__T_string(t75)
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
    var t79 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t79)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv82 string
    var t83 string = _goml_runtime_core_int32_to_string(self__41)
    retv82 = t83
    return retv82
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv85 string
    var t86 string = _goml_runtime_core_bool_to_string(self__36)
    retv85 = t86
    return retv85
}

func id__T_int32(x__6 int32) int32 {
    var retv88 int32
    retv88 = x__6
    return retv88
}

func id__T_string(x__6 string) string {
    var retv90 string
    retv90 = x__6
    return retv90
}

func id__T_bool(x__6 bool) bool {
    var retv92 bool
    retv92 = x__6
    return retv92
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv94 string
    retv94 = self__37
    return retv94
}

func main() {
    main0()
}
