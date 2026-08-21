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

type S struct {}

type Ordering int32

func _goml_m_trait__impl_i_A_i_S_i_pick(self__0 S) int32 {
    return 10
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    return 20
}

func main0() struct{} {
    var t418 S = S{}
    var t419 int32
    var inline451 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(t418)
    t419 = inline451
    var t420 string
    var inline449 string = _goml_runtime_core_int32_to_string(t419)
    t420 = inline449
    var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline446)
    var t421 S = S{}
    var t422 int32
    var inline444 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(t421)
    t422 = inline444
    var t423 string
    var inline442 string = _goml_runtime_core_int32_to_string(t422)
    t423 = inline442
    var inline439 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline439)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
