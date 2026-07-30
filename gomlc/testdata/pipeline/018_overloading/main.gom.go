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
    var retv113 int32
    var t114 int32 = self__0 + other__1
    retv113 = t114
    return retv113
}

func _goml_m_trait__impl_i_Arith_i_int32_i_less(self__2 int32, other__3 int32) bool {
    var retv116 bool
    var t117 bool = self__2 < other__3
    retv116 = t117
    return retv116
}

func _goml_m_trait__impl_i_Output_i_int32_i_output(self__4 int32) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__4)
    println__T_string(t119)
    return struct{}{}
}

func _goml_m_trait__impl_i_Output_i_bool_i_output(self__5 bool) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__5)
    println__T_string(t122)
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
    var t126 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv129 string
    var t130 string = _goml_runtime_core_int32_to_string(self__43)
    retv129 = t130
    return retv129
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv132 string
    var t133 string = _goml_runtime_core_bool_to_string(self__37)
    retv132 = t133
    return retv132
}

func id__T_int32(x__6 int32) int32 {
    var retv135 int32
    retv135 = x__6
    return retv135
}

func id__T_string(x__6 string) string {
    var retv137 string
    retv137 = x__6
    return retv137
}

func id__T_bool(x__6 bool) bool {
    var retv139 bool
    retv139 = x__6
    return retv139
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv141 string
    retv141 = self__38
    return retv141
}

func main() {
    main0()
}
