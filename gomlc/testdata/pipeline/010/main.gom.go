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
    var x136 bool = true
    var x137 bool = true
    switch x137 {
    case true:
        switch x136 {
        case true:
            var t141 string
            var inline162 int = 789
            var inline163 string = _goml_runtime_core_int_to_string(inline162)
            t141 = inline163
            var inline159 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t141)
            _goml_runtime_core_string_println(inline159)
            return struct{}{}
        case false:
            var t143 string
            var inline168 int = 456
            var inline169 string = _goml_runtime_core_int_to_string(inline168)
            t143 = inline169
            var inline165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t143)
            _goml_runtime_core_string_println(inline165)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x136 {
        case true:
            var t146 string
            var inline174 int = 123
            var inline175 string = _goml_runtime_core_int_to_string(inline174)
            t146 = inline175
            var inline171 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
            _goml_runtime_core_string_println(inline171)
            return struct{}{}
        case false:
            var t148 string
            var inline180 int = 789
            var inline181 string = _goml_runtime_core_int_to_string(inline180)
            t148 = inline181
            var inline177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
            _goml_runtime_core_string_println(inline177)
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
