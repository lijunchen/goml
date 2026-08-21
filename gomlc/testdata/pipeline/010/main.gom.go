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

type Ordering int32

func main0() struct{} {
    var x411 bool = true
    var x412 bool = true
    switch x412 {
    case true:
        switch x411 {
        case true:
            var t416 string
            var inline437 int = 789
            var inline438 string = _goml_runtime_core_int_to_string(inline437)
            t416 = inline438
            var inline434 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
            _goml_runtime_core_string_println(inline434)
            return struct{}{}
        case false:
            var t418 string
            var inline443 int = 456
            var inline444 string = _goml_runtime_core_int_to_string(inline443)
            t418 = inline444
            var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
            _goml_runtime_core_string_println(inline440)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x411 {
        case true:
            var t421 string
            var inline449 int = 123
            var inline450 string = _goml_runtime_core_int_to_string(inline449)
            t421 = inline450
            var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
            _goml_runtime_core_string_println(inline446)
            return struct{}{}
        case false:
            var t423 string
            var inline455 int = 789
            var inline456 string = _goml_runtime_core_int_to_string(inline455)
            t423 = inline456
            var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
            _goml_runtime_core_string_println(inline452)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
