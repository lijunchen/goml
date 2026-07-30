package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_m_range(start__0 int32, end__1 int32) int32 {
    var retv116 int32
    var t117 int32 = start__0 + end__1
    retv116 = t117
    return retv116
}

func main0() struct{} {
    var for_index108 int = 1
    var for_limit109 int = 4
    Loop_loop121:
    for {
        var t122 bool = for_index108 < for_limit109
        if t122 {
            var for_item110 int = for_index108
            var t123 int = for_index108 + 1
            for_index108 = t123
            var value__2 int = for_item110
            println__T_int(value__2)
            continue
        } else {
            break Loop_loop121
        }
    }
    var t120 int32 = _goml_m_range(10, 20)
    println__T_int32(t120)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t125 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t125)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t128 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t128)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv131 string
    var t132 string = _goml_runtime_core_int_to_string(self__40)
    retv131 = t132
    return retv131
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv134 string
    var t135 string = _goml_runtime_core_int32_to_string(self__43)
    retv134 = t135
    return retv134
}

func main() {
    main0()
}
