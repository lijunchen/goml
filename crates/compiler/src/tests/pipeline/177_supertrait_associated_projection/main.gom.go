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
    var retv60 int32
    var t61 int32 = self__0.value
    retv60 = t61
    return retv60
}

func main0() struct{} {
    var t64 Count = Count{
        value: 42,
    }
    var t65 int32 = get_from_child__C_Count(t64)
    println__T_int32(t65)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t67 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t67)
    return struct{}{}
}

func get_from_child__C_Count(value__2 Count) int32 {
    var retv70 int32
    var t71 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(value__2)
    retv70 = t71
    return retv70
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int32_to_string(self__38)
    retv73 = t74
    return retv73
}

func main() {
    main0()
}
