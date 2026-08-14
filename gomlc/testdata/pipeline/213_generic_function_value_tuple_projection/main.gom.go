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
    var inline431 int32 = 1
    var inline432 string = "x"
    var inline433 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline431,
        _1: inline432,
    }
    value__3 = inline433
    var t409 int32 = value__3._0
    var t410 string
    var inline429 string = _goml_runtime_core_int32_to_string(t409)
    t410 = inline429
    var t411 string = value__3._1
    var t412 string = t410 + t411
    var inline426 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t412)
    _goml_runtime_core_string_println(inline426)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
