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
    var t157 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(sum16__2)
    var t158 string = t157 + ", "
    var t159 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(flipped16__3)
    var t160 string = t158 + t159
    var t161 string = t160 + "; "
    var t162 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff32__7)
    var t163 string = t161 + t162
    var t164 string = t163 + "; "
    var t165 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(remain64__10)
    var t166 string = t164 + t165
    var t167 string = t166 + "; "
    var t168 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(neg64__11)
    var message__12 string = t167 + t168
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var t171 string = _goml_runtime_core_int16_to_string(self__42)
    return t171
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t174 string = _goml_runtime_core_int32_to_string(self__6)
    return t174
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__44 int64) string {
    var t177 string = _goml_runtime_core_int64_to_string(self__44)
    return t177
}

func println__T_string(value__1 string) struct{} {
    var t179 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t179)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
