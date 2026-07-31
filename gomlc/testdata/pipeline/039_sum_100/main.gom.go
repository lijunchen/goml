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
    var retv153 bool
    var t156 bool = x__0 < y__1
    var t157 bool = !t156
    var jp155 bool
    if t157 {
        var t158 bool = y__1 < x__0
        var t159 bool = !t158
        jp155 = t159
    } else {
        jp155 = false
    }
    retv153 = jp155
    return retv153
}

func sum(n__2 int32) int32 {
    var retv161 int32
    var t164 bool = my_int_equal(n__2, 1)
    var jp163 int32
    if t164 {
        jp163 = 1
    } else {
        var t165 int32 = n__2 - 1
        var t166 int32 = sum(t165)
        var t167 int32 = n__2 + t166
        jp163 = t167
    }
    retv161 = jp163
    return retv161
}

func main0() struct{} {
    var t169 int32 = sum(100)
    println__T_int32(t169)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv175 string
    var t176 string = _goml_runtime_core_int32_to_string(self__43)
    retv175 = t176
    return retv175
}

func main() {
    main0()
}
