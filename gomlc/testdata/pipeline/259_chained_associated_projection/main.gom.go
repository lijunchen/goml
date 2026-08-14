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

type NumberIterator struct {}

type Numbers struct {}

type Ordering int32

func main0() struct{} {
    var t411 int32
    var inline425 int32 = 42
    t411 = inline425
    var inline422 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t411)
    _goml_runtime_core_string_println(inline422)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t420 string = _goml_runtime_core_int32_to_string(self__154)
    return t420
}

func main() {
    main0()
}
