package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var x__0 int = 1
    var t185 int = x__0 + 1
    x__0 = t185
    var t186 string
    var inline199 string = _goml_runtime_core_int_to_string(x__0)
    t186 = inline199
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline196)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
