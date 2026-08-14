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

type Ordering int32

func _goml_m_trait__impl_i_Parent_i_Count_i_get(self__0 Count) int32 {
    var t411 int32 = self__0.value
    return t411
}

func main0() struct{} {
    var t414 Count = Count{
        value: 42,
    }
    var t415 int32
    var inline429 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(t414)
    t415 = inline429
    var inline426 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t415)
    _goml_runtime_core_string_println(inline426)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t424 string = _goml_runtime_core_int32_to_string(self__154)
    return t424
}

func main() {
    main0()
}
