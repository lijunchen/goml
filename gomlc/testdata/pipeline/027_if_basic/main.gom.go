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

type Ordering int32

func main0() struct{} {
    var yes__3 int32
    var inline442 bool = true
    var inline443 int32 = 10
    var inline444 int32 = 99
    if inline442 {
        yes__3 = inline443
    } else {
        yes__3 = inline444
    }
    var no__4 int32
    var inline438 bool = false
    var inline439 int32 = 10
    var inline440 int32 = 99
    if inline438 {
        no__4 = inline439
    } else {
        no__4 = inline440
    }
    var t415 string
    var inline436 string = _goml_runtime_core_int32_to_string(yes__3)
    t415 = inline436
    var t416 string = "yes=" + t415
    var inline433 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
    _goml_runtime_core_string_println(inline433)
    var t417 string
    var inline431 string = _goml_runtime_core_int32_to_string(no__4)
    t417 = inline431
    var t418 string = "no=" + t417
    var inline428 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline428)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
