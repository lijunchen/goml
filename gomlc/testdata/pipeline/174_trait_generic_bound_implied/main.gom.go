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

type NumberBox struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var t414 string
    var inline435 string = _goml_runtime_core_int32_to_string(self__0)
    t414 = inline435
    var t415 string = "marked:" + t414
    return t415
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t418 int32 = self__1.value
    return t418
}

func main0() struct{} {
    var t420 NumberBox = NumberBox{
        value: 7,
    }
    var t421 string
    var inline440 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(t420)
    var inline441 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline440)
    t421 = inline441
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline437)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
