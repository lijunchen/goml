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
    var t184 string
    var inline225 string = _goml_runtime_core_uint8_to_string(sum8__2)
    t184 = inline225
    var t185 string = t184 + ", "
    var t186 string
    var inline223 string = _goml_runtime_core_uint8_to_string(neg8__3)
    t186 = inline223
    var t187 string = t185 + t186
    var t188 string = t187 + "; "
    var t189 string
    var inline221 string = _goml_runtime_core_uint16_to_string(diff16__7)
    t189 = inline221
    var t190 string = t188 + t189
    var t191 string = t190 + "; "
    var t192 string
    var inline219 string = _goml_runtime_core_uint32_to_string(neg32__11)
    t192 = inline219
    var t193 string = t191 + t192
    var t194 string = t193 + "; "
    var t195 string
    var inline217 string = _goml_runtime_core_uint64_to_string(diff64__15)
    t195 = inline217
    var message__16 string = t194 + t195
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__16)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
