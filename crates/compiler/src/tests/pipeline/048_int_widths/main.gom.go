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
    var t63 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(sum16__2)
    var t64 string = t63 + ", "
    var t65 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(flipped16__3)
    var t66 string = t64 + t65
    var t67 string = t66 + "; "
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff32__7)
    var t69 string = t67 + t68
    var t70 string = t69 + "; "
    var t71 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(remain64__10)
    var t72 string = t70 + t71
    var t73 string = t72 + "; "
    var t74 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(neg64__11)
    var message__12 string = t73 + t74
    println__T_string(message__12)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__40 int16) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int16_to_string(self__40)
    retv76 = t77
    return retv76
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__5)
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__42 int64) string {
    var retv82 string
    var t83 string = _goml_runtime_core_int64_to_string(self__42)
    retv82 = t83
    return retv82
}

func println__T_string(value__1 string) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv88 string
    retv88 = self__37
    return retv88
}

func main() {
    main0()
}
