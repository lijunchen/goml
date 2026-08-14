package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var x187 bool = true
    var x188 bool = true
    switch x188 {
    case true:
        switch x187 {
        case true:
            var t192 string
            var inline213 int = 789
            var inline214 string = _goml_runtime_core_int_to_string(inline213)
            t192 = inline214
            var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
            _goml_runtime_core_string_println(inline210)
            return struct{}{}
        case false:
            var t194 string
            var inline219 int = 456
            var inline220 string = _goml_runtime_core_int_to_string(inline219)
            t194 = inline220
            var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
            _goml_runtime_core_string_println(inline216)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x187 {
        case true:
            var t197 string
            var inline225 int = 123
            var inline226 string = _goml_runtime_core_int_to_string(inline225)
            t197 = inline226
            var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
            _goml_runtime_core_string_println(inline222)
            return struct{}{}
        case false:
            var t199 string
            var inline231 int = 789
            var inline232 string = _goml_runtime_core_int_to_string(inline231)
            t199 = inline232
            var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
            _goml_runtime_core_string_println(inline228)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
