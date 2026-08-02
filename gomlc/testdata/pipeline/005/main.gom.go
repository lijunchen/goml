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
    var x155 Color = Blue
    var x156 Color = Red
    switch x156 {
    case Red:
        switch x155 {
        case Red:
            var inline175 int = 1
            var inline176 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline175)
            _goml_runtime_core_string_print(inline176)
            return struct{}{}
        default:
            var inline179 int = 3
            var inline180 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline179)
            _goml_runtime_core_string_print(inline180)
            return struct{}{}
        }
    case Green:
        switch x155 {
        case Red:
            var inline183 int = 0
            var inline184 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline183)
            _goml_runtime_core_string_print(inline184)
            return struct{}{}
        default:
            var inline187 int = 3
            var inline188 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline187)
            _goml_runtime_core_string_print(inline188)
            return struct{}{}
        }
    case Blue:
        switch x155 {
        case Blue:
            var inline191 int = 2
            var inline192 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline191)
            _goml_runtime_core_string_print(inline192)
            return struct{}{}
        default:
            var inline195 int = 3
            var inline196 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline195)
            _goml_runtime_core_string_print(inline196)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t173 string = _goml_runtime_core_int_to_string(self__40)
    return t173
}

func main() {
    main0()
}
