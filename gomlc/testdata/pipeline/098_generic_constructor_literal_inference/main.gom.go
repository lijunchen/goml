package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Option__uint8 struct {
    _tag int32
    _v0_0 uint8
}

func main0() struct{} {
    var x411 uint8 = 42
    var t415 string
    var inline430 string = _goml_runtime_core_uint8_to_string(x411)
    t415 = inline430
    var inline427 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline427)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
