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
        var t195 string
        var inline224 int = 1
        var inline225 string = _goml_runtime_core_int_to_string(inline224)
        t195 = inline225
        var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
        _goml_runtime_core_string_println(inline221)
        return struct{}{}
    case B:
        var x187 bool = t__0.(B)._0
        var x188 bool = t__0.(B)._1
        switch x188 {
        case true:
            switch x187 {
            case true:
                var t199 string
                var inline230 int = 4
                var inline231 string = _goml_runtime_core_int_to_string(inline230)
                t199 = inline231
                var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
                _goml_runtime_core_string_println(inline227)
                return struct{}{}
            case false:
                var t201 string
                var inline236 int = 3
                var inline237 string = _goml_runtime_core_int_to_string(inline236)
                t201 = inline237
                var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline233)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x187 {
            case true:
                var t204 string
                var inline242 int = 4
                var inline243 string = _goml_runtime_core_int_to_string(inline242)
                t204 = inline243
                var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
                _goml_runtime_core_string_println(inline239)
                return struct{}{}
            case false:
                var t206 string
                var inline248 int = 2
                var inline249 string = _goml_runtime_core_int_to_string(inline248)
                t206 = inline249
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
                _goml_runtime_core_string_println(inline245)
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
    var t209 T = B{
        _0: true,
        _1: true,
    }
    test(t209)
    var t210 T = B{
        _0: false,
        _1: true,
    }
    test(t210)
    var t211 T = B{
        _0: false,
        _1: false,
    }
    test(t211)
    test(A{})
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
