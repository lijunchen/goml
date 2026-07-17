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
    var t63 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t63)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv66 string
    retv66 = self__34
    return retv66
}

func main() {
    main0()
}
