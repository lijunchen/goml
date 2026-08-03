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
    var t138 string
    var inline179 string = _goml_runtime_core_uint8_to_string(sum8__2)
    t138 = inline179
    var t139 string = t138 + ", "
    var t140 string
    var inline177 string = _goml_runtime_core_uint8_to_string(neg8__3)
    t140 = inline177
    var t141 string = t139 + t140
    var t142 string = t141 + "; "
    var t143 string
    var inline175 string = _goml_runtime_core_uint16_to_string(diff16__7)
    t143 = inline175
    var t144 string = t142 + t143
    var t145 string = t144 + "; "
    var t146 string
    var inline173 string = _goml_runtime_core_uint32_to_string(neg32__11)
    t146 = inline173
    var t147 string = t145 + t146
    var t148 string = t147 + "; "
    var t149 string
    var inline171 string = _goml_runtime_core_uint64_to_string(diff64__15)
    t149 = inline171
    var message__16 string = t148 + t149
    var inline168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__16)
    _goml_runtime_core_string_println(inline168)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
