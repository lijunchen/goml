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
    var a__0 int = 1
    var a__1 int = a__0 + 2
    var a__2 int = a__1 + 3
    var a__3 int = a__2 + 4
    var t183 string
    var inline197 string = _goml_runtime_core_int_to_string(a__3)
    t183 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
