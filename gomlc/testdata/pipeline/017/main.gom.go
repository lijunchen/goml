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
    var t188 string
    var inline215 string = _goml_runtime_core_int_to_string(x__1)
    t188 = inline215
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline212)
    var x__2 bool = true
    var t189 string
    var inline210 string = _goml_runtime_core_bool_to_string(x__2)
    t189 = inline210
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline207)
    var t190 string
    t190 = "(?, ?)"
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline203)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
