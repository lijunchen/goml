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
    var x408 bool = true
    switch x408 {
    case true:
        var t416 string
        var inline432 int = 2
        var inline433 string = _goml_runtime_core_int_to_string(inline432)
        t416 = inline433
        var inline429 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
        _goml_runtime_core_string_println(inline429)
        return struct{}{}
    case false:
        var t418 string
        var inline438 int = 3
        var inline439 string = _goml_runtime_core_int_to_string(inline438)
        t418 = inline439
        var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
        _goml_runtime_core_string_println(inline435)
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
