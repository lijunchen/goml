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
    var t6 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(sum16__2)
    var t7 string = t6 + ", "
    var t8 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(flipped16__3)
    var t9 string = t7 + t8
    var t10 string = t9 + "; "
    var t11 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff32__7)
    var t12 string = t10 + t11
    var t13 string = t12 + "; "
    var t14 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(remain64__10)
    var t15 string = t13 + t14
    var t16 string = t15 + "; "
    var t17 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(neg64__11)
    var message__12 string = t16 + t17
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__12 int16) string {
    var retv19 string
    var t20 string = _goml_runtime_core_int16_to_string(self__12)
    retv19 = t20
    return retv19
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv22 string
    var t23 string = _goml_runtime_core_int32_to_string(self__2)
    retv22 = t23
    return retv22
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__14 int64) string {
    var retv25 string
    var t26 string = _goml_runtime_core_int64_to_string(self__14)
    retv25 = t26
    return retv25
}

func println__T_string(value__1 string) struct{} {
    var t28 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t28)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv31 string
    retv31 = self__9
    return retv31
}

func main() {
    main0()
}
