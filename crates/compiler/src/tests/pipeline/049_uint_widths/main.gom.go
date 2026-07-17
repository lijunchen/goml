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
    var t60 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(sum8__2)
    var t61 string = t60 + ", "
    var t62 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(neg8__3)
    var t63 string = t61 + t62
    var t64 string = t63 + "; "
    var t65 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(diff16__7)
    var t66 string = t64 + t65
    var t67 string = t66 + "; "
    var t68 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(neg32__11)
    var t69 string = t67 + t68
    var t70 string = t69 + "; "
    var t71 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(diff64__15)
    var message__16 string = t70 + t71
    println__T_string(message__16)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__40 uint8) string {
    var retv73 string
    var t74 string = _goml_runtime_core_uint8_to_string(self__40)
    retv73 = t74
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__41 uint16) string {
    var retv76 string
    var t77 string = _goml_runtime_core_uint16_to_string(self__41)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__42 uint32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_uint32_to_string(self__42)
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__43 uint64) string {
    var retv82 string
    var t83 string = _goml_runtime_core_uint64_to_string(self__43)
    retv82 = t83
    return retv82
}

func println__T_string(value__1 string) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv88 string
    retv88 = self__34
    return retv88
}

func main() {
    main0()
}
