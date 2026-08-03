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
    var t179 string
    var inline217 string = _goml_runtime_core_int16_to_string(sum16__2)
    t179 = inline217
    var t180 string = t179 + ", "
    var t181 string
    var inline215 string = _goml_runtime_core_int16_to_string(flipped16__3)
    t181 = inline215
    var t182 string = t180 + t181
    var t183 string = t182 + "; "
    var t184 string
    var inline213 string = _goml_runtime_core_int32_to_string(diff32__7)
    t184 = inline213
    var t185 string = t183 + t184
    var t186 string = t185 + "; "
    var t187 string
    var inline211 string = _goml_runtime_core_int64_to_string(remain64__10)
    t187 = inline211
    var t188 string = t186 + t187
    var t189 string = t188 + "; "
    var t190 string
    var inline209 string = _goml_runtime_core_int64_to_string(neg64__11)
    t190 = inline209
    var message__12 string = t189 + t190
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
