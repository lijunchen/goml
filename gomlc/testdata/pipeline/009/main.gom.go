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
        var t185 string
        var inline214 int = 1
        var inline215 string = _goml_runtime_core_int_to_string(inline214)
        t185 = inline215
        var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
        _goml_runtime_core_string_println(inline211)
        return struct{}{}
    case B:
        var x177 bool = t__0.(B)._0
        var x178 bool = t__0.(B)._1
        switch x178 {
        case true:
            switch x177 {
            case true:
                var t189 string
                var inline220 int = 4
                var inline221 string = _goml_runtime_core_int_to_string(inline220)
                t189 = inline221
                var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
                _goml_runtime_core_string_println(inline217)
                return struct{}{}
            case false:
                var t191 string
                var inline226 int = 3
                var inline227 string = _goml_runtime_core_int_to_string(inline226)
                t191 = inline227
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
                _goml_runtime_core_string_println(inline223)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x177 {
            case true:
                var t194 string
                var inline232 int = 4
                var inline233 string = _goml_runtime_core_int_to_string(inline232)
                t194 = inline233
                var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
                _goml_runtime_core_string_println(inline229)
                return struct{}{}
            case false:
                var t196 string
                var inline238 int = 2
                var inline239 string = _goml_runtime_core_int_to_string(inline238)
                t196 = inline239
                var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline235)
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
    var t199 T = B{
        _0: true,
        _1: true,
    }
    test(t199)
    var t200 T = B{
        _0: false,
        _1: true,
    }
    test(t200)
    var t201 T = B{
        _0: false,
        _1: false,
    }
    test(t201)
    test(A{})
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
