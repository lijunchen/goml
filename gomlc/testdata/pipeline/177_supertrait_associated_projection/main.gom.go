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
    var t190 int32 = self__0.value
    return t190
}

func main0() struct{} {
    var t193 Count = Count{
        value: 42,
    }
    var t194 int32
    var inline208 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(t193)
    t194 = inline208
    var inline205 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t194)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t203 string = _goml_runtime_core_int32_to_string(self__70)
    return t203
}

func main() {
    main0()
}
