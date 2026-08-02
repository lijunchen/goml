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
        var t163 string
        var inline192 int = 1
        var inline193 string = _goml_runtime_core_int_to_string(inline192)
        t163 = inline193
        var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
        _goml_runtime_core_string_println(inline189)
        return struct{}{}
    case B:
        var x155 bool = t__0.(B)._0
        var x156 bool = t__0.(B)._1
        switch x156 {
        case true:
            switch x155 {
            case true:
                var t167 string
                var inline198 int = 4
                var inline199 string = _goml_runtime_core_int_to_string(inline198)
                t167 = inline199
                var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
                _goml_runtime_core_string_println(inline195)
                return struct{}{}
            case false:
                var t169 string
                var inline204 int = 3
                var inline205 string = _goml_runtime_core_int_to_string(inline204)
                t169 = inline205
                var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
                _goml_runtime_core_string_println(inline201)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x155 {
            case true:
                var t172 string
                var inline210 int = 4
                var inline211 string = _goml_runtime_core_int_to_string(inline210)
                t172 = inline211
                var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
                _goml_runtime_core_string_println(inline207)
                return struct{}{}
            case false:
                var t174 string
                var inline216 int = 2
                var inline217 string = _goml_runtime_core_int_to_string(inline216)
                t174 = inline217
                var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
                _goml_runtime_core_string_println(inline213)
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
    var t177 T = B{
        _0: true,
        _1: true,
    }
    test(t177)
    var t178 T = B{
        _0: false,
        _1: true,
    }
    test(t178)
    var t179 T = B{
        _0: false,
        _1: false,
    }
    test(t179)
    test(A{})
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
