package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Ordering int32

func fib(x__0 int32) int32 {
    var mtmp411 bool = x__0 < 2
    switch mtmp411 {
    case true:
        return 1
    case false:
        var t417 int32 = x__0 - 1
        var t418 int32 = fib(t417)
        var t419 int32 = x__0 - 2
        var t420 int32 = fib(t419)
        var t421 int32 = t418 + t420
        return t421
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t423 int32 = fib(10)
    var inline431 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t423)
    _goml_runtime_core_string_print(inline431)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t429 string = _goml_runtime_core_int32_to_string(self__154)
    return t429
}

func main() {
    main0()
}
