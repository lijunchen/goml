package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var s__0 string = "abcde"
    println__T_string(s__0)
    print__T_string(s__0)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t62 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t62)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t65 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t65)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv68 string
    retv68 = self__34
    return retv68
}

func main() {
    main0()
}
