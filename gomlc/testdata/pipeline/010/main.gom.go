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
    var x408 bool = true
    var x409 bool = true
    switch x409 {
    case true:
        switch x408 {
        case true:
            var t413 string
            var inline434 int = 789
            var inline435 string = _goml_runtime_core_int_to_string(inline434)
            t413 = inline435
            var inline431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t413)
            _goml_runtime_core_string_println(inline431)
            return struct{}{}
        case false:
            var t415 string
            var inline440 int = 456
            var inline441 string = _goml_runtime_core_int_to_string(inline440)
            t415 = inline441
            var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
            _goml_runtime_core_string_println(inline437)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x408 {
        case true:
            var t418 string
            var inline446 int = 123
            var inline447 string = _goml_runtime_core_int_to_string(inline446)
            t418 = inline447
            var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
            _goml_runtime_core_string_println(inline443)
            return struct{}{}
        case false:
            var t420 string
            var inline452 int = 789
            var inline453 string = _goml_runtime_core_int_to_string(inline452)
            t420 = inline453
            var inline449 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
            _goml_runtime_core_string_println(inline449)
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
