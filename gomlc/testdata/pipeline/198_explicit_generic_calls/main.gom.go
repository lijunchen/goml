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
    var t70 string = identity__T_string("direct")
    _goml_runtime_core_string_println(t70)
    var t71 int32 = identity__T_int32(42)
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t71)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func identity__T_string(value__0 string) string {
    var retv75 string
    retv75 = value__0
    return retv75
}

func identity__T_int32(value__0 int32) int32 {
    var retv77 int32
    retv77 = value__0
    return retv77
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__6)
    retv79 = t80
    return retv79
}

func main() {
    main0()
}
