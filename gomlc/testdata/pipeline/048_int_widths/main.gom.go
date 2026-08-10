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
    var t174 string
    var inline212 string = _goml_runtime_core_int16_to_string(sum16__2)
    t174 = inline212
    var t175 string = t174 + ", "
    var t176 string
    var inline210 string = _goml_runtime_core_int16_to_string(flipped16__3)
    t176 = inline210
    var t177 string = t175 + t176
    var t178 string = t177 + "; "
    var t179 string
    var inline208 string = _goml_runtime_core_int32_to_string(diff32__7)
    t179 = inline208
    var t180 string = t178 + t179
    var t181 string = t180 + "; "
    var t182 string
    var inline206 string = _goml_runtime_core_int64_to_string(remain64__10)
    t182 = inline206
    var t183 string = t181 + t182
    var t184 string = t183 + "; "
    var t185 string
    var inline204 string = _goml_runtime_core_int64_to_string(neg64__11)
    t185 = inline204
    var message__12 string = t184 + t185
    var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline201)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
