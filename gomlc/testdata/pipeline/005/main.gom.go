package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type Tuple2_5Color_5Color struct {
    _0 Color
    _1 Color
}

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() struct{} {
    var x177 Color = Blue
    var x178 Color = Red
    switch x178 {
    case Red:
        switch x177 {
        case Red:
            var inline197 int = 1
            var inline198 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline197)
            _goml_runtime_core_string_print(inline198)
            return struct{}{}
        default:
            var inline201 int = 3
            var inline202 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline201)
            _goml_runtime_core_string_print(inline202)
            return struct{}{}
        }
    case Green:
        switch x177 {
        case Red:
            var inline205 int = 0
            var inline206 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline205)
            _goml_runtime_core_string_print(inline206)
            return struct{}{}
        default:
            var inline209 int = 3
            var inline210 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline209)
            _goml_runtime_core_string_print(inline210)
            return struct{}{}
        }
    case Blue:
        switch x177 {
        case Blue:
            var inline213 int = 2
            var inline214 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline213)
            _goml_runtime_core_string_print(inline214)
            return struct{}{}
        default:
            var inline217 int = 3
            var inline218 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline217)
            _goml_runtime_core_string_print(inline218)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t195 string = _goml_runtime_core_int_to_string(self__69)
    return t195
}

func main() {
    main0()
}
