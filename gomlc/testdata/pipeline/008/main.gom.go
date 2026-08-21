package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type T struct {
    _tag int32
    _v1_0 bool
    _v1_1 struct{}
}

func main0() struct{} {
    var x411 bool = true
    switch x411 {
    case true:
        var t419 string
        var inline435 int = 2
        var inline436 string = _goml_runtime_core_int_to_string(inline435)
        t419 = inline436
        var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
        _goml_runtime_core_string_println(inline432)
        return struct{}{}
    case false:
        var t421 string
        var inline441 int = 3
        var inline442 string = _goml_runtime_core_int_to_string(inline441)
        t421 = inline442
        var inline438 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
        _goml_runtime_core_string_println(inline438)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
