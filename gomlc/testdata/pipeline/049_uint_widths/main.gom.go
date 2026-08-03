package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var start8__0 uint8 = 200
    var add8__1 uint8 = 55
    var sum8__2 uint8 = start8__0 + add8__1
    var neg8__3 uint8 = -start8__0
    var start16__4 uint16 = 50000
    var add16__5 uint16 = 12000
    var sum16__6 uint16 = start16__4 + add16__5
    var diff16__7 uint16 = sum16__6 - start16__4
    var add32__9 uint32 = 123456789
    var neg32__11 uint32 = -add32__9
    var start64__12 uint64 = 6000000000
    var add64__13 uint64 = 4000000000
    var sum64__14 uint64 = start64__12 + add64__13
    var diff64__15 uint64 = sum64__14 - add64__13
    var t179 string
    var inline220 string = _goml_runtime_core_uint8_to_string(sum8__2)
    t179 = inline220
    var t180 string = t179 + ", "
    var t181 string
    var inline218 string = _goml_runtime_core_uint8_to_string(neg8__3)
    t181 = inline218
    var t182 string = t180 + t181
    var t183 string = t182 + "; "
    var t184 string
    var inline216 string = _goml_runtime_core_uint16_to_string(diff16__7)
    t184 = inline216
    var t185 string = t183 + t184
    var t186 string = t185 + "; "
    var t187 string
    var inline214 string = _goml_runtime_core_uint32_to_string(neg32__11)
    t187 = inline214
    var t188 string = t186 + t187
    var t189 string = t188 + "; "
    var t190 string
    var inline212 string = _goml_runtime_core_uint64_to_string(diff64__15)
    t190 = inline212
    var message__16 string = t189 + t190
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__16)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
