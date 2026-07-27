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
    for {
        if flag__0 {
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
    return struct{}{}
}

func loop_return() int32 {
    var retv77 int32
    retv77 = 5
    return retv77
}

func main0() struct{} {
    loop_return_unit(true)
    var t81 int32 = loop_return()
    println__T_int32(t81)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t86 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t86)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__43)
    retv91 = t92
    return retv91
}

func main() {
    main0()
}
