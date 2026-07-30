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

type Count struct {
    value int32
}

func _goml_m_trait__impl_i_Parent_i_Count_i_get(self__0 Count) int32 {
    var retv110 int32
    var t111 int32 = self__0.value
    retv110 = t111
    return retv110
}

func main0() struct{} {
    var t114 Count = Count{
        value: 42,
    }
    var t115 int32 = get_from_child__C_Count(t114)
    println__T_int32(t115)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t117 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t117)
    return struct{}{}
}

func get_from_child__C_Count(value__2 Count) int32 {
    var retv120 int32
    var t121 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(value__2)
    retv120 = t121
    return retv120
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv123 string
    var t124 string = _goml_runtime_core_int32_to_string(self__43)
    retv123 = t124
    return retv123
}

func main() {
    main0()
}
