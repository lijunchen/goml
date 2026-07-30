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

func main0() struct{} {
    var t110 string = identity__T_string("direct")
    _goml_runtime_core_string_println(t110)
    var t111 int32 = identity__T_int32(42)
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t111)
    _goml_runtime_core_string_println(t112)
    return struct{}{}
}

func identity__T_string(value__0 string) string {
    var retv115 string
    retv115 = value__0
    return retv115
}

func identity__T_int32(value__0 int32) int32 {
    var retv117 int32
    retv117 = value__0
    return retv117
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv119 string
    var t120 string = _goml_runtime_core_int32_to_string(self__6)
    retv119 = t120
    return retv119
}

func main() {
    main0()
}
