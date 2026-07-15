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
    var t24 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(sum8__2)
    var t25 string = t24 + ", "
    var t26 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(neg8__3)
    var t27 string = t25 + t26
    var t28 string = t27 + "; "
    var t29 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(diff16__7)
    var t30 string = t28 + t29
    var t31 string = t30 + "; "
    var t32 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(neg32__11)
    var t33 string = t31 + t32
    var t34 string = t33 + "; "
    var t35 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(diff64__15)
    var message__16 string = t34 + t35
    println__T_string(message__16)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__15 uint8) string {
    var retv37 string
    var t38 string = _goml_runtime_core_uint8_to_string(self__15)
    retv37 = t38
    return retv37
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__16 uint16) string {
    var retv40 string
    var t41 string = _goml_runtime_core_uint16_to_string(self__16)
    retv40 = t41
    return retv40
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__17 uint32) string {
    var retv43 string
    var t44 string = _goml_runtime_core_uint32_to_string(self__17)
    retv43 = t44
    return retv43
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__18 uint64) string {
    var retv46 string
    var t47 string = _goml_runtime_core_uint64_to_string(self__18)
    retv46 = t47
    return retv46
}

func println__T_string(value__1 string) struct{} {
    var t49 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t49)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv52 string
    retv52 = self__9
    return retv52
}

func main() {
    main0()
}
