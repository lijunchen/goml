package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func my_int_equal(x__0 int32, y__1 int32) bool {
    var retv109 bool
    var t112 bool = x__0 < y__1
    var t113 bool = !t112
    var jp111 bool
    if t113 {
        var t114 bool = y__1 < x__0
        var t115 bool = !t114
        jp111 = t115
    } else {
        jp111 = false
    }
    retv109 = jp111
    return retv109
}

func sum(n__2 int32) int32 {
    var retv117 int32
    var t120 bool = my_int_equal(n__2, 1)
    var jp119 int32
    if t120 {
        jp119 = 1
    } else {
        var t121 int32 = n__2 - 1
        var t122 int32 = sum(t121)
        var t123 int32 = n__2 + t122
        jp119 = t123
    }
    retv117 = jp119
    return retv117
}

func main0() struct{} {
    var t125 int32 = sum(100)
    println__T_int32(t125)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t128 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t128)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv131 string
    var t132 string = _goml_runtime_core_int32_to_string(self__43)
    retv131 = t132
    return retv131
}

func main() {
    main0()
}
