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

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

func main0() struct{} {
    var x177 bool = true
    var x178 bool = true
    switch x178 {
    case true:
        switch x177 {
        case true:
            var t182 string
            var inline203 int = 789
            var inline204 string = _goml_runtime_core_int_to_string(inline203)
            t182 = inline204
            var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
            _goml_runtime_core_string_println(inline200)
            return struct{}{}
        case false:
            var t184 string
            var inline209 int = 456
            var inline210 string = _goml_runtime_core_int_to_string(inline209)
            t184 = inline210
            var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
            _goml_runtime_core_string_println(inline206)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x177 {
        case true:
            var t187 string
            var inline215 int = 123
            var inline216 string = _goml_runtime_core_int_to_string(inline215)
            t187 = inline216
            var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
            _goml_runtime_core_string_println(inline212)
            return struct{}{}
        case false:
            var t189 string
            var inline221 int = 789
            var inline222 string = _goml_runtime_core_int_to_string(inline221)
            t189 = inline222
            var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
            _goml_runtime_core_string_println(inline218)
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
