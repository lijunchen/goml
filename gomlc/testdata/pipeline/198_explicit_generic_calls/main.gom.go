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
    var t157 string = identity__T_string("direct")
    _goml_runtime_core_string_println(t157)
    var t158 int32 = identity__T_int32(42)
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t158)
    _goml_runtime_core_string_println(t159)
    return struct{}{}
}

func identity__T_string(value__0 string) string {
    var retv162 string
    retv162 = value__0
    return retv162
}

func identity__T_int32(value__0 int32) int32 {
    var retv164 int32
    retv164 = value__0
    return retv164
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv166 string
    var t167 string = _goml_runtime_core_int32_to_string(self__6)
    retv166 = t167
    return retv166
}

func main() {
    main0()
}
