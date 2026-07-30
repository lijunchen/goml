package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Buffer struct {
    values [3]int32
}

func main0() struct{} {
    print__T_string("array")
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t113 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t113)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv116 string
    retv116 = self__38
    return retv116
}

func main() {
    main0()
}
