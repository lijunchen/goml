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
    var t154 string = identity__T_string("direct")
    _goml_runtime_core_string_println(t154)
    var t155 int32 = identity__T_int32(42)
    var t156 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t155)
    _goml_runtime_core_string_println(t156)
    return struct{}{}
}

func identity__T_string(value__0 string) string {
    var retv159 string
    retv159 = value__0
    return retv159
}

func identity__T_int32(value__0 int32) int32 {
    var retv161 int32
    retv161 = value__0
    return retv161
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv163 string
    var t164 string = _goml_runtime_core_int32_to_string(self__6)
    retv163 = t164
    return retv163
}

func main() {
    main0()
}
