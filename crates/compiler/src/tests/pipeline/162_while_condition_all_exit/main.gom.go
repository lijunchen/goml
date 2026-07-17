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
    var retv71 int32
    retv71 = 5
    return retv71
}

func main0() struct{} {
    loop_return_unit(true)
    var t75 int32 = loop_return()
    println__T_int32(t75)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv85 string
    var t86 string = _goml_runtime_core_int32_to_string(self__38)
    retv85 = t86
    return retv85
}

func main() {
    main0()
}
