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
    var inline445 bool = true
    var inline446 int32 = 10
    var inline447 int32 = 99
    if inline445 {
        yes__3 = inline446
    } else {
        yes__3 = inline447
    }
    var no__4 int32
    var inline441 bool = false
    var inline442 int32 = 10
    var inline443 int32 = 99
    if inline441 {
        no__4 = inline442
    } else {
        no__4 = inline443
    }
    var t418 string
    var inline439 string = _goml_runtime_core_int32_to_string(yes__3)
    t418 = inline439
    var t419 string = "yes=" + t418
    var inline436 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline436)
    var t420 string
    var inline434 string = _goml_runtime_core_int32_to_string(no__4)
    t420 = inline434
    var t421 string = "no=" + t420
    var inline431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline431)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
