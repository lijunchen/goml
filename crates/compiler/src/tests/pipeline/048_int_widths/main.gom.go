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
    var t24 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(sum16__2)
    var t25 string = t24 + ", "
    var t26 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(flipped16__3)
    var t27 string = t25 + t26
    var t28 string = t27 + "; "
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff32__7)
    var t30 string = t28 + t29
    var t31 string = t30 + "; "
    var t32 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(remain64__10)
    var t33 string = t31 + t32
    var t34 string = t33 + "; "
    var t35 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(neg64__11)
    var message__12 string = t34 + t35
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__12 int16) string {
    var retv37 string
    var t38 string = _goml_runtime_core_int16_to_string(self__12)
    retv37 = t38
    return retv37
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__2)
    retv40 = t41
    return retv40
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv43 string
    var t44 string = _goml_runtime_core_int64_to_string(self__14)
    retv43 = t44
    return retv43
}

func println__T_string(value__1 string) struct{} {
    var t46 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t46)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv49 string
    retv49 = self__9
    return retv49
}

func main() {
    main0()
}
