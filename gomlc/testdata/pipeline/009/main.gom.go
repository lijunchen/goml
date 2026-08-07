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
        var t180 string
        var inline209 int = 1
        var inline210 string = _goml_runtime_core_int_to_string(inline209)
        t180 = inline210
        var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
        _goml_runtime_core_string_println(inline206)
        return struct{}{}
    case B:
        var x172 bool = t__0.(B)._0
        var x173 bool = t__0.(B)._1
        switch x173 {
        case true:
            switch x172 {
            case true:
                var t184 string
                var inline215 int = 4
                var inline216 string = _goml_runtime_core_int_to_string(inline215)
                t184 = inline216
                var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
                _goml_runtime_core_string_println(inline212)
                return struct{}{}
            case false:
                var t186 string
                var inline221 int = 3
                var inline222 string = _goml_runtime_core_int_to_string(inline221)
                t186 = inline222
                var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
                _goml_runtime_core_string_println(inline218)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x172 {
            case true:
                var t189 string
                var inline227 int = 4
                var inline228 string = _goml_runtime_core_int_to_string(inline227)
                t189 = inline228
                var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
                _goml_runtime_core_string_println(inline224)
                return struct{}{}
            case false:
                var t191 string
                var inline233 int = 2
                var inline234 string = _goml_runtime_core_int_to_string(inline233)
                t191 = inline234
                var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
                _goml_runtime_core_string_println(inline230)
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
    var t194 T = B{
        _0: true,
        _1: true,
    }
    test(t194)
    var t195 T = B{
        _0: false,
        _1: true,
    }
    test(t195)
    var t196 T = B{
        _0: false,
        _1: false,
    }
    test(t196)
    test(A{})
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
