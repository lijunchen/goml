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

type Point struct {
    x int32
    y int32
}

type Ordering int32

type Maybe__int32 interface {
    isMaybe__int32()
}

type Just struct {
    _0 int32
}

func (_ Just) isMaybe__int32() {}

type Nothing struct {}

func (_ Nothing) isMaybe__int32() {}

func main0() struct{} {
    var some_number__5 Maybe__int32
    var inline466 bool = true
    if inline466 {
        var inline467 Maybe__int32 = Just{
            _0: 42,
        }
        some_number__5 = inline467
    } else {
        some_number__5 = Nothing{}
    }
    var none_number__6 Maybe__int32
    var inline463 bool = false
    if inline463 {
        var inline464 Maybe__int32 = Just{
            _0: 42,
        }
        none_number__6 = inline464
    } else {
        none_number__6 = Nothing{}
    }
    var t427 string
    t427 = "Point"
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline459)
    var t428 string
    switch some_number__5.(type) {
    case Just:
        var inline453 int32 = some_number__5.(Just)._0
        var inline455 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline453)
        var inline456 string = "Just(" + inline455
        var inline457 string = inline456 + ")"
        t428 = inline457
    case Nothing:
        t428 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline450)
    var t429 string
    switch none_number__6.(type) {
    case Just:
        var inline444 int32 = none_number__6.(Just)._0
        var inline446 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline444)
        var inline447 string = "Just(" + inline446
        var inline448 string = inline447 + ")"
        t429 = inline448
    case Nothing:
        t429 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline441 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline441)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t432 string = _goml_runtime_core_int32_to_string(self__33)
    return t432
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
