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
    var t420 int32
    switch light__2 {
    case Light_Red:
        t420 = 10
    case Yellow:
        t420 = 20
    case Green:
        t420 = 30
    default:
        panic("non-exhaustive match")
    }
    var t421 string
    var inline459 string = _goml_runtime_core_int32_to_string(t420)
    t421 = inline459
    var inline456 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline456)
    var t422 int32
    switch paint__3 {
    case Paint_Red:
        t422 = 1
    case Blue:
        t422 = 2
    default:
        panic("non-exhaustive match")
    }
    var t423 string
    var inline453 string = _goml_runtime_core_int32_to_string(t422)
    t423 = inline453
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline450)
    var t424 int32
    t424 = 30
    var t425 string
    var inline447 string = _goml_runtime_core_int32_to_string(t424)
    t425 = inline447
    var inline444 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline444)
    var t426 int32
    t426 = 2
    var t427 string
    var inline441 string = _goml_runtime_core_int32_to_string(t426)
    t427 = inline441
    var inline438 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline438)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
