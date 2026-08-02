package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func make_array() [3]int32 {
    var t160 [3]int32 = [3]int32{1, 2, 3}
    return t160
}

func main0() struct{} {
    make_array()
    print__T_string("array literal")
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t163)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
