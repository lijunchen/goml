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
    var t175 int32 = self__0.value
    return t175
}

func main0() struct{} {
    var t178 Count = Count{
        value: 42,
    }
    var t179 int32
    var inline193 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(t178)
    t179 = inline193
    var inline190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t179)
    _goml_runtime_core_string_println(inline190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t188 string = _goml_runtime_core_int32_to_string(self__72)
    return t188
}

func main() {
    main0()
}
