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
    var retv157 int32
    var t158 int32 = self__0.value
    retv157 = t158
    return retv157
}

func main0() struct{} {
    var t161 Count = Count{
        value: 42,
    }
    var t162 int32 = get_from_child__C_Count(t161)
    println__T_int32(t162)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t164 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t164)
    return struct{}{}
}

func get_from_child__C_Count(value__2 Count) int32 {
    var retv167 int32
    var t168 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(value__2)
    retv167 = t168
    return retv167
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int32_to_string(self__43)
    retv170 = t171
    return retv170
}

func main() {
    main0()
}
