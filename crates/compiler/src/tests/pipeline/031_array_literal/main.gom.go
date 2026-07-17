package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func make_array() [3]int32 {
    var retv62 [3]int32
    var t63 [3]int32 = [3]int32{1, 2, 3}
    retv62 = t63
    return retv62
}

func main0() struct{} {
    make_array()
    print__T_string("array literal")
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t66 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t66)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv69 string
    retv69 = self__34
    return retv69
}

func main() {
    main0()
}
