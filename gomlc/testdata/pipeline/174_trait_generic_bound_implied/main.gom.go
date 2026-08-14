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
    var t411 string
    var inline432 string = _goml_runtime_core_int32_to_string(self__0)
    t411 = inline432
    var t412 string = "marked:" + t411
    return t412
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t415 int32 = self__1.value
    return t415
}

func main0() struct{} {
    var t417 NumberBox = NumberBox{
        value: 7,
    }
    var t418 string
    var inline437 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(t417)
    var inline438 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline437)
    t418 = inline438
    var inline434 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline434)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
