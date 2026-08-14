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
    var t189 string
    var inline227 string = _goml_runtime_core_int16_to_string(sum16__2)
    t189 = inline227
    var t190 string = t189 + ", "
    var t191 string
    var inline225 string = _goml_runtime_core_int16_to_string(flipped16__3)
    t191 = inline225
    var t192 string = t190 + t191
    var t193 string = t192 + "; "
    var t194 string
    var inline223 string = _goml_runtime_core_int32_to_string(diff32__7)
    t194 = inline223
    var t195 string = t193 + t194
    var t196 string = t195 + "; "
    var t197 string
    var inline221 string = _goml_runtime_core_int64_to_string(remain64__10)
    t197 = inline221
    var t198 string = t196 + t197
    var t199 string = t198 + "; "
    var t200 string
    var inline219 string = _goml_runtime_core_int64_to_string(neg64__11)
    t200 = inline219
    var message__12 string = t199 + t200
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
