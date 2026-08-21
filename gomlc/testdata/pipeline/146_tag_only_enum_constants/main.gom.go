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

type Light int32

const (
    Light_Red Light = 0
    Yellow Light = 1
    Green Light = 2
)

type Paint int32

const (
    Paint_Red Paint = 0
    Blue Paint = 1
)

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t423 int32
    switch light__2 {
    case Light_Red:
        t423 = 10
    case Yellow:
        t423 = 20
    case Green:
        t423 = 30
    default:
        panic("non-exhaustive match")
    }
    var t424 string
    var inline462 string = _goml_runtime_core_int32_to_string(t423)
    t424 = inline462
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline459)
    var t425 int32
    switch paint__3 {
    case Paint_Red:
        t425 = 1
    case Blue:
        t425 = 2
    default:
        panic("non-exhaustive match")
    }
    var t426 string
    var inline456 string = _goml_runtime_core_int32_to_string(t425)
    t426 = inline456
    var inline453 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline453)
    var t427 int32
    t427 = 30
    var t428 string
    var inline450 string = _goml_runtime_core_int32_to_string(t427)
    t428 = inline450
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline447)
    var t429 int32
    t429 = 2
    var t430 string
    var inline444 string = _goml_runtime_core_int32_to_string(t429)
    t430 = inline444
    var inline441 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline441)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
