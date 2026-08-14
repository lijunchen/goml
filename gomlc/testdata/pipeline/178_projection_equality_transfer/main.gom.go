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

type LeftSource struct {
    value int32
}

type RightSource struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Mark_i_int32_i_marked(self__0 int32) string {
    var t411 string
    var inline440 string = _goml_runtime_core_int32_to_string(self__0)
    t411 = inline440
    var t412 string = "m" + t411
    return t412
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t415 int32 = self__1.value
    return t415
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t418 int32 = self__2.value
    return t418
}

func main0() struct{} {
    var t420 LeftSource = LeftSource{
        value: 3,
    }
    var t421 RightSource = RightSource{
        value: 4,
    }
    var t422 string
    var inline445 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t420)
    var inline446 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline445)
    var inline447 string = inline446 + ":"
    var inline448 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t421)
    var inline449 string = _goml_m_trait__impl_i_Mark_i_int32_i_marked(inline448)
    var inline450 string = inline447 + inline449
    t422 = inline450
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline442)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
