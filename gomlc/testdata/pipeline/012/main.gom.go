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
    var mtmp408 bool = x__0 < 2
    switch mtmp408 {
    case true:
        return 1
    case false:
        var t414 int32 = x__0 - 1
        var t415 int32 = fib(t414)
        var t416 int32 = x__0 - 2
        var t417 int32 = fib(t416)
        var t418 int32 = t415 + t417
        return t418
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t420 int32 = fib(10)
    var inline428 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t420)
    _goml_runtime_core_string_print(inline428)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t426 string = _goml_runtime_core_int32_to_string(self__154)
    return t426
}

func main() {
    main0()
}
