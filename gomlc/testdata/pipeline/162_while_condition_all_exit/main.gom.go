package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func loop_return_unit(flag__0 bool) struct{} {
    if flag__0 {
        return struct{}{}
    } else {
        return struct{}{}
    }
}

func loop_return() int32 {
    return 5
}

func main0() struct{} {
    loop_return_unit(true)
    var t199 int32 = loop_return()
    var inline212 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
    _goml_runtime_core_string_println(inline212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t210 string = _goml_runtime_core_int32_to_string(self__70)
    return t210
}

func main() {
    main0()
}
