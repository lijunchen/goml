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
    var retv160 int32
    var t161 int32 = start__0 + end__1
    retv160 = t161
    return retv160
}

func main0() struct{} {
    var for_index152 int = 1
    var for_limit153 int = 4
    Loop_loop165:
    for {
        var t166 bool = for_index152 < for_limit153
        if t166 {
            var for_item154 int = for_index152
            var t167 int = for_index152 + 1
            for_index152 = t167
            var value__2 int = for_item154
            println__T_int(value__2)
            continue
        } else {
            break Loop_loop165
        }
    }
    var t164 int32 = _goml_m_range(10, 20)
    println__T_int32(t164)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t169)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv175 string
    var t176 string = _goml_runtime_core_int_to_string(self__40)
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv178 string
    var t179 string = _goml_runtime_core_int32_to_string(self__43)
    retv178 = t179
    return retv178
}

func main() {
    main0()
}
