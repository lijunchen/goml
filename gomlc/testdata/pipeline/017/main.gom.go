package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

func main0() struct{} {
    var x__1 int = 123
    var t142 string
    var inline169 string = _goml_runtime_core_int_to_string(x__1)
    t142 = inline169
    var inline166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t142)
    _goml_runtime_core_string_println(inline166)
    var x__2 bool = true
    var t143 string
    var inline164 string = _goml_runtime_core_bool_to_string(x__2)
    t143 = inline164
    var inline161 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t143)
    _goml_runtime_core_string_println(inline161)
    var t144 string
    t144 = "(?, ?)"
    var inline157 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t144)
    _goml_runtime_core_string_println(inline157)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
