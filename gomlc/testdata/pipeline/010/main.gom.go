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
    var x155 bool = true
    var x156 bool = true
    switch x156 {
    case true:
        switch x155 {
        case true:
            var t160 string
            var inline181 int = 789
            var inline182 string = _goml_runtime_core_int_to_string(inline181)
            t160 = inline182
            var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
            _goml_runtime_core_string_println(inline178)
            return struct{}{}
        case false:
            var t162 string
            var inline187 int = 456
            var inline188 string = _goml_runtime_core_int_to_string(inline187)
            t162 = inline188
            var inline184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
            _goml_runtime_core_string_println(inline184)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x155 {
        case true:
            var t165 string
            var inline193 int = 123
            var inline194 string = _goml_runtime_core_int_to_string(inline193)
            t165 = inline194
            var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
            _goml_runtime_core_string_println(inline190)
            return struct{}{}
        case false:
            var t167 string
            var inline199 int = 789
            var inline200 string = _goml_runtime_core_int_to_string(inline199)
            t167 = inline200
            var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
            _goml_runtime_core_string_println(inline196)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
