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
    var t184 string
    var inline222 string = _goml_runtime_core_int16_to_string(sum16__2)
    t184 = inline222
    var t185 string = t184 + ", "
    var t186 string
    var inline220 string = _goml_runtime_core_int16_to_string(flipped16__3)
    t186 = inline220
    var t187 string = t185 + t186
    var t188 string = t187 + "; "
    var t189 string
    var inline218 string = _goml_runtime_core_int32_to_string(diff32__7)
    t189 = inline218
    var t190 string = t188 + t189
    var t191 string = t190 + "; "
    var t192 string
    var inline216 string = _goml_runtime_core_int64_to_string(remain64__10)
    t192 = inline216
    var t193 string = t191 + t192
    var t194 string = t193 + "; "
    var t195 string
    var inline214 string = _goml_runtime_core_int64_to_string(neg64__11)
    t195 = inline214
    var message__12 string = t194 + t195
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
