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
    var retv24 int32
    var t25 int32 = self__0.value
    retv24 = t25
    return retv24
}

func main0() struct{} {
    var t28 Count = Count{
        value: 42,
    }
    var t29 int32 = get_from_child__C_Count(t28)
    println__T_int32(t29)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t31 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t31)
    return struct{}{}
}

func get_from_child__C_Count(value__2 Count) int32 {
    var retv34 int32
    var t35 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(value__2)
    retv34 = t35
    return retv34
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv37 string
    var t38 string = _goml_runtime_core_int32_to_string(self__13)
    retv37 = t38
    return retv37
}

func main() {
    main0()
}
