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
    var retv76 int32
    var t77 int32 = start__0 + end__1
    retv76 = t77
    return retv76
}

func main0() struct{} {
    var for_index68 int = 1
    var for_limit69 int = 4
    Loop_loop81:
    for {
        var t82 bool = for_index68 < for_limit69
        if t82 {
            var for_item70 int = for_index68
            var t83 int = for_index68 + 1
            for_index68 = t83
            var value__2 int = for_item70
            println__T_int(value__2)
            continue
        } else {
            break Loop_loop81
        }
    }
    var t80 int32 = _goml_m_range(10, 20)
    println__T_int32(t80)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int_to_string(self__40)
    retv91 = t92
    return retv91
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__43)
    retv94 = t95
    return retv94
}

func main() {
    main0()
}
