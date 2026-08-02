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
    var t157 string
    var inline198 string = _goml_runtime_core_uint8_to_string(sum8__2)
    t157 = inline198
    var t158 string = t157 + ", "
    var t159 string
    var inline196 string = _goml_runtime_core_uint8_to_string(neg8__3)
    t159 = inline196
    var t160 string = t158 + t159
    var t161 string = t160 + "; "
    var t162 string
    var inline194 string = _goml_runtime_core_uint16_to_string(diff16__7)
    t162 = inline194
    var t163 string = t161 + t162
    var t164 string = t163 + "; "
    var t165 string
    var inline192 string = _goml_runtime_core_uint32_to_string(neg32__11)
    t165 = inline192
    var t166 string = t164 + t165
    var t167 string = t166 + "; "
    var t168 string
    var inline190 string = _goml_runtime_core_uint64_to_string(diff64__15)
    t168 = inline190
    var message__16 string = t167 + t168
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__16)
    _goml_runtime_core_string_println(inline187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
