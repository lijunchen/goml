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

func sum(n__2 int32) int32 {
    var t423 bool
    var inline437 int32 = 1
    var inline438 bool = n__2 < inline437
    var inline439 bool = !inline438
    if inline439 {
        var inline440 bool = inline437 < n__2
        var inline441 bool = !inline440
        t423 = inline441
    } else {
        t423 = false
    }
    if t423 {
        return 1
    } else {
        var t424 int32 = n__2 - 1
        var t425 int32 = sum(t424)
        var t426 int32 = n__2 + t425
        return t426
    }
}

func main0() struct{} {
    var t428 int32 = sum(100)
    var inline443 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t428)
    _goml_runtime_core_string_println(inline443)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t435 string = _goml_runtime_core_int32_to_string(self__154)
    return t435
}

func main() {
    main0()
}
