package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var start16__0 int16 = 300
    var delta16__1 int16 = 45
    var sum16__2 int16 = start16__0 + delta16__1
    var flipped16__3 int16 = -start16__0
    var base32__4 int32 = 100000
    var more32__5 int32 = 200000
    var sum32__6 int32 = base32__4 + more32__5
    var diff32__7 int32 = sum32__6 - base32__4
    var big64__8 int64 = 5000000000
    var step64__9 int64 = 2000000000
    var remain64__10 int64 = big64__8 - step64__9
    var neg64__11 int64 = -step64__9
    var t110 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(sum16__2)
    var t111 string = t110 + ", "
    var t112 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(flipped16__3)
    var t113 string = t111 + t112
    var t114 string = t113 + "; "
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff32__7)
    var t116 string = t114 + t115
    var t117 string = t116 + "; "
    var t118 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(remain64__10)
    var t119 string = t117 + t118
    var t120 string = t119 + "; "
    var t121 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(neg64__11)
    var message__12 string = t120 + t121
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv123 string
    var t124 string = _goml_runtime_core_int16_to_string(self__42)
    retv123 = t124
    return retv123
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int32_to_string(self__6)
    retv126 = t127
    return retv126
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var retv129 string
    var t130 string = _goml_runtime_core_int64_to_string(self__44)
    retv129 = t130
    return retv129
}

func println__T_string(value__1 string) struct{} {
    var t132 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t132)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv135 string
    retv135 = self__38
    return retv135
}

func main() {
    main0()
}
