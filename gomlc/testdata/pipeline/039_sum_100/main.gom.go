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
    var retv156 bool
    var t159 bool = x__0 < y__1
    var t160 bool = !t159
    var jp158 bool
    if t160 {
        var t161 bool = y__1 < x__0
        var t162 bool = !t161
        jp158 = t162
    } else {
        jp158 = false
    }
    retv156 = jp158
    return retv156
}

func sum(n__2 int32) int32 {
    var retv164 int32
    var t167 bool = my_int_equal(n__2, 1)
    var jp166 int32
    if t167 {
        jp166 = 1
    } else {
        var t168 int32 = n__2 - 1
        var t169 int32 = sum(t168)
        var t170 int32 = n__2 + t169
        jp166 = t170
    }
    retv164 = jp166
    return retv164
}

func main0() struct{} {
    var t172 int32 = sum(100)
    println__T_int32(t172)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t175 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t175)
    return struct{}{}
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
