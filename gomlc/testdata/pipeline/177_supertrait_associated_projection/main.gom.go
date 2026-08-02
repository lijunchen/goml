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
    var t158 int32 = self__0.value
    return t158
}

func main0() struct{} {
    var t161 Count = Count{
        value: 42,
    }
    var t162 int32
    var inline176 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(t161)
    t162 = inline176
    var inline173 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t162)
    _goml_runtime_core_string_println(inline173)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t171 string = _goml_runtime_core_int32_to_string(self__43)
    return t171
}

func main() {
    main0()
}
