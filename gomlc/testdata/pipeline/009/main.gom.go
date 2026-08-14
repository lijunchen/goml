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

type T interface {
    isT()
}

type A struct {}

func (_ A) isT() {}

type B struct {
    _0 bool
    _1 bool
}

func (_ B) isT() {}

func test(t__0 T) struct{} {
    switch t__0.(type) {
    case A:
        var t190 string
        var inline219 int = 1
        var inline220 string = _goml_runtime_core_int_to_string(inline219)
        t190 = inline220
        var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
        _goml_runtime_core_string_println(inline216)
        return struct{}{}
    case B:
        var x182 bool = t__0.(B)._0
        var x183 bool = t__0.(B)._1
        switch x183 {
        case true:
            switch x182 {
            case true:
                var t194 string
                var inline225 int = 4
                var inline226 string = _goml_runtime_core_int_to_string(inline225)
                t194 = inline226
                var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            case false:
                var t196 string
                var inline231 int = 3
                var inline232 string = _goml_runtime_core_int_to_string(inline231)
                t196 = inline232
                var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline228)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x182 {
            case true:
                var t199 string
                var inline237 int = 4
                var inline238 string = _goml_runtime_core_int_to_string(inline237)
                t199 = inline238
                var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
                _goml_runtime_core_string_println(inline234)
                return struct{}{}
            case false:
                var t201 string
                var inline243 int = 2
                var inline244 string = _goml_runtime_core_int_to_string(inline243)
                t201 = inline244
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline240)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t204 T = B{
        _0: true,
        _1: true,
    }
    test(t204)
    var t205 T = B{
        _0: false,
        _1: true,
    }
    test(t205)
    var t206 T = B{
        _0: false,
        _1: false,
    }
    test(t206)
    test(A{})
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
