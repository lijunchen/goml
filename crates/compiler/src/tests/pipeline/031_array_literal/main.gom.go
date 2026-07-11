package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func make_array() [3]int32 {
    var retv8 [3]int32
    var t9 [3]int32 = [3]int32{1, 2, 3}
    retv8 = t9
    return retv8
}

func main0() struct{} {
    make_array()
    print__T_string("array literal")
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t12 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t12)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv15 string
    retv15 = self__9
    return retv15
}

func main() {
    main0()
}
