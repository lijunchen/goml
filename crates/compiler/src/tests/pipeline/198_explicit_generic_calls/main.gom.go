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
    var t63 string = identity__T_string("direct")
    _goml_runtime_core_string_println(t63)
    var t64 int32 = identity__T_int32(42)
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t64)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func identity__T_string(value__0 string) string {
    var retv68 string
    retv68 = value__0
    return retv68
}

func identity__T_int32(value__0 int32) int32 {
    var retv70 int32
    retv70 = value__0
    return retv70
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv72 string
    var t73 string = _goml_runtime_core_int32_to_string(self__5)
    retv72 = t73
    return retv72
}

func main() {
    main0()
}
