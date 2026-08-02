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
    var t157 string
    var inline195 string = _goml_runtime_core_int16_to_string(sum16__2)
    t157 = inline195
    var t158 string = t157 + ", "
    var t159 string
    var inline193 string = _goml_runtime_core_int16_to_string(flipped16__3)
    t159 = inline193
    var t160 string = t158 + t159
    var t161 string = t160 + "; "
    var t162 string
    var inline191 string = _goml_runtime_core_int32_to_string(diff32__7)
    t162 = inline191
    var t163 string = t161 + t162
    var t164 string = t163 + "; "
    var t165 string
    var inline189 string = _goml_runtime_core_int64_to_string(remain64__10)
    t165 = inline189
    var t166 string = t164 + t165
    var t167 string = t166 + "; "
    var t168 string
    var inline187 string = _goml_runtime_core_int64_to_string(neg64__11)
    t168 = inline187
    var message__12 string = t167 + t168
    var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
