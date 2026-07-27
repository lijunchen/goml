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
    var t66 string = identity__T_string("direct")
    _goml_runtime_core_string_println(t66)
    var t67 int32 = identity__T_int32(42)
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t67)
    _goml_runtime_core_string_println(t68)
    return struct{}{}
}

func identity__T_string(value__0 string) string {
    var retv71 string
    retv71 = value__0
    return retv71
}

func identity__T_int32(value__0 int32) int32 {
    var retv73 int32
    retv73 = value__0
    return retv73
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv75 string
    var t76 string = _goml_runtime_core_int32_to_string(self__6)
    retv75 = t76
    return retv75
}

func main() {
    main0()
}
