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
    var t420 bool
    var inline434 int32 = 1
    var inline435 bool = n__2 < inline434
    var inline436 bool = !inline435
    if inline436 {
        var inline437 bool = inline434 < n__2
        var inline438 bool = !inline437
        t420 = inline438
    } else {
        t420 = false
    }
    if t420 {
        return 1
    } else {
        var t421 int32 = n__2 - 1
        var t422 int32 = sum(t421)
        var t423 int32 = n__2 + t422
        return t423
    }
}

func main0() struct{} {
    var t425 int32 = sum(100)
    var inline440 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t425)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t432 string = _goml_runtime_core_int32_to_string(self__154)
    return t432
}

func main() {
    main0()
}
