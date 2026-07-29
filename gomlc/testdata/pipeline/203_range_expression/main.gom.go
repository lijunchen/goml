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
    var retv72 int32
    var t73 int32 = start__0 + end__1
    retv72 = t73
    return retv72
}

func main0() struct{} {
    var for_index64 int = 1
    var for_limit65 int = 4
    Loop_loop77:
    for {
        var t78 bool = for_index64 < for_limit65
        if t78 {
            var for_item66 int = for_index64
            var t79 int = for_index64 + 1
            for_index64 = t79
            var value__2 int = for_item66
            println__T_int(value__2)
            continue
        } else {
            break Loop_loop77
        }
    }
    var t76 int32 = _goml_m_range(10, 20)
    println__T_int32(t76)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int_to_string(self__40)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv90 string
    var t91 string = _goml_runtime_core_int32_to_string(self__43)
    retv90 = t91
    return retv90
}

func main() {
    main0()
}
