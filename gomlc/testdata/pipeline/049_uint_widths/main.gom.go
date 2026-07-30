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
    var t70 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(sum8__2)
    var t71 string = t70 + ", "
    var t72 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(neg8__3)
    var t73 string = t71 + t72
    var t74 string = t73 + "; "
    var t75 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(diff16__7)
    var t76 string = t74 + t75
    var t77 string = t76 + "; "
    var t78 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(neg32__11)
    var t79 string = t77 + t78
    var t80 string = t79 + "; "
    var t81 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(diff64__15)
    var message__16 string = t80 + t81
    println__T_string(message__16)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv83 string
    var t84 string = _goml_runtime_core_uint8_to_string(self__45)
    retv83 = t84
    return retv83
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__46 uint16) string {
    var retv86 string
    var t87 string = _goml_runtime_core_uint16_to_string(self__46)
    retv86 = t87
    return retv86
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__47 uint32) string {
    var retv89 string
    var t90 string = _goml_runtime_core_uint32_to_string(self__47)
    retv89 = t90
    return retv89
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv92 string
    var t93 string = _goml_runtime_core_uint64_to_string(self__48)
    retv92 = t93
    return retv92
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv98 string
    retv98 = self__38
    return retv98
}

func main() {
    main0()
}
