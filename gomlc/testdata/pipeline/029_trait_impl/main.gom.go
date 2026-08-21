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

type Maybe__int32 struct {
    _tag int32
    _v0_0 int32
}

func main0() struct{} {
    var some_number__5 Maybe__int32
    var inline469 bool = true
    if inline469 {
        var inline470 Maybe__int32 = Maybe__int32{
            _tag: 0,
            _v0_0: 42,
        }
        some_number__5 = inline470
    } else {
        some_number__5 = Maybe__int32{
            _tag: 1,
        }
    }
    var none_number__6 Maybe__int32
    var inline466 bool = false
    if inline466 {
        var inline467 Maybe__int32 = Maybe__int32{
            _tag: 0,
            _v0_0: 42,
        }
        none_number__6 = inline467
    } else {
        none_number__6 = Maybe__int32{
            _tag: 1,
        }
    }
    var t430 string
    t430 = "Point"
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline462)
    var t431 string
    switch some_number__5._tag {
    case 0:
        var inline456 int32 = some_number__5._v0_0
        var inline458 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline456)
        var inline459 string = "Just(" + inline458
        var inline460 string = inline459 + ")"
        t431 = inline460
    case 1:
        t431 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline453 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline453)
    var t432 string
    switch none_number__6._tag {
    case 0:
        var inline447 int32 = none_number__6._v0_0
        var inline449 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline447)
        var inline450 string = "Just(" + inline449
        var inline451 string = inline450 + ")"
        t432 = inline451
    case 1:
        t432 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline444)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t435 string = _goml_runtime_core_int32_to_string(self__33)
    return t435
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
