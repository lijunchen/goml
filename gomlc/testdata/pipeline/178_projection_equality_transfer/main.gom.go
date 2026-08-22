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

func _goml_m_trait__impl_i_Mark_i_i32_i_marked(self__0 int32) string {
    var t414 string
    var inline443 string = _goml_runtime_core_int32_to_string(self__0)
    t414 = inline443
    var t415 string = "m" + t414
    return t415
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t418 int32 = self__1.value
    return t418
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t421 int32 = self__2.value
    return t421
}

func main0() struct{} {
    var t423 LeftSource = LeftSource{
        value: 3,
    }
    var t424 RightSource = RightSource{
        value: 4,
    }
    var t425 string
    var inline448 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t423)
    var inline449 string = _goml_m_trait__impl_i_Mark_i_i32_i_marked(inline448)
    var inline450 string = inline449 + ":"
    var inline451 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t424)
    var inline452 string = _goml_m_trait__impl_i_Mark_i_i32_i_marked(inline451)
    var inline453 string = inline450 + inline452
    t425 = inline453
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline445)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
