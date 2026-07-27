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
    var retv66 int32
    var t67 int32 = self__0.value
    retv66 = t67
    return retv66
}

func main0() struct{} {
    var t70 Count = Count{
        value: 42,
    }
    var t71 int32 = get_from_child__C_Count(t70)
    println__T_int32(t71)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func get_from_child__C_Count(value__2 Count) int32 {
    var retv76 int32
    var t77 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(value__2)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__43)
    retv79 = t80
    return retv79
}

func main() {
    main0()
}
