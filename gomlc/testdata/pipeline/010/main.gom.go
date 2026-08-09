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
    var x172 bool = true
    var x173 bool = true
    switch x173 {
    case true:
        switch x172 {
        case true:
            var t177 string
            var inline198 int = 789
            var inline199 string = _goml_runtime_core_int_to_string(inline198)
            t177 = inline199
            var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
            _goml_runtime_core_string_println(inline195)
            return struct{}{}
        case false:
            var t179 string
            var inline204 int = 456
            var inline205 string = _goml_runtime_core_int_to_string(inline204)
            t179 = inline205
            var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
            _goml_runtime_core_string_println(inline201)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x172 {
        case true:
            var t182 string
            var inline210 int = 123
            var inline211 string = _goml_runtime_core_int_to_string(inline210)
            t182 = inline211
            var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
            _goml_runtime_core_string_println(inline207)
            return struct{}{}
        case false:
            var t184 string
            var inline216 int = 789
            var inline217 string = _goml_runtime_core_int_to_string(inline216)
            t184 = inline217
            var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
            _goml_runtime_core_string_println(inline213)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
