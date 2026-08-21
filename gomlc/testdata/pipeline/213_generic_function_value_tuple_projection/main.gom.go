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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
}

type Ordering int32

func main0() struct{} {
    var value__3 Tuple2_5int32_6string
    var inline434 int32 = 1
    var inline435 string = "x"
    var inline436 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline434,
        _1: inline435,
    }
    value__3 = inline436
    var t412 int32 = value__3._0
    var t413 string
    var inline432 string = _goml_runtime_core_int32_to_string(t412)
    t413 = inline432
    var t414 string = value__3._1
    var t415 string = t413 + t414
    var inline429 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline429)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
