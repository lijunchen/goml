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
    var t414 int32 = self__0.value
    return t414
}

func main0() struct{} {
    var t417 Count = Count{
        value: 42,
    }
    var t418 int32
    var inline432 int32 = _goml_m_trait__impl_i_Parent_i_Count_i_get(t417)
    t418 = inline432
    var inline429 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t418)
    _goml_runtime_core_string_println(inline429)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t427 string = _goml_runtime_core_int32_to_string(self__154)
    return t427
}

func main() {
    main0()
}
