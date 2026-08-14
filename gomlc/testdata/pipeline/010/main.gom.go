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
    var x182 bool = true
    var x183 bool = true
    switch x183 {
    case true:
        switch x182 {
        case true:
            var t187 string
            var inline208 int = 789
            var inline209 string = _goml_runtime_core_int_to_string(inline208)
            t187 = inline209
            var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
            _goml_runtime_core_string_println(inline205)
            return struct{}{}
        case false:
            var t189 string
            var inline214 int = 456
            var inline215 string = _goml_runtime_core_int_to_string(inline214)
            t189 = inline215
            var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
            _goml_runtime_core_string_println(inline211)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x182 {
        case true:
            var t192 string
            var inline220 int = 123
            var inline221 string = _goml_runtime_core_int_to_string(inline220)
            t192 = inline221
            var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
            _goml_runtime_core_string_println(inline217)
            return struct{}{}
        case false:
            var t194 string
            var inline226 int = 789
            var inline227 string = _goml_runtime_core_int_to_string(inline226)
            t194 = inline227
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
            _goml_runtime_core_string_println(inline223)
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
