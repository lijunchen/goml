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

func main0() struct{} {
    var a__0 int32 = 1
    var a__1 int32 = a__0 + 2
    var a__2 int32 = a__1 + 3
    var a__3 int32 = a__2 + 4
    var t5 string = _goml_m_inherent_i_int32_i_int32_i_to__string(a__3)
    println__T_string(t5)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t8 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t8)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv11 string
    var t12 string = _goml_runtime_core_int32_to_string(self__2)
    retv11 = t12
    return retv11
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv14 string
    retv14 = self__9
    return retv14
}

func main() {
    main0()
}
