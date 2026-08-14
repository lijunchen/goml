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

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() struct{} {
    var x182 Color = Blue
    var x183 Color = Red
    switch x183 {
    case Red:
        switch x182 {
        case Red:
            var inline202 int = 1
            var inline203 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline202)
            _goml_runtime_core_string_print(inline203)
            return struct{}{}
        default:
            var inline206 int = 3
            var inline207 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline206)
            _goml_runtime_core_string_print(inline207)
            return struct{}{}
        }
    case Green:
        switch x182 {
        case Red:
            var inline210 int = 0
            var inline211 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline210)
            _goml_runtime_core_string_print(inline211)
            return struct{}{}
        default:
            var inline214 int = 3
            var inline215 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline214)
            _goml_runtime_core_string_print(inline215)
            return struct{}{}
        }
    case Blue:
        switch x182 {
        case Blue:
            var inline218 int = 2
            var inline219 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline218)
            _goml_runtime_core_string_print(inline219)
            return struct{}{}
        default:
            var inline222 int = 3
            var inline223 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline222)
            _goml_runtime_core_string_print(inline223)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t200 string = _goml_runtime_core_int_to_string(self__67)
    return t200
}

func main() {
    main0()
}
