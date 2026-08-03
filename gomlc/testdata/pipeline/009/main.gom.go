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
        var t144 string
        var inline173 int = 1
        var inline174 string = _goml_runtime_core_int_to_string(inline173)
        t144 = inline174
        var inline170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t144)
        _goml_runtime_core_string_println(inline170)
        return struct{}{}
    case B:
        var x136 bool = t__0.(B)._0
        var x137 bool = t__0.(B)._1
        switch x137 {
        case true:
            switch x136 {
            case true:
                var t148 string
                var inline179 int = 4
                var inline180 string = _goml_runtime_core_int_to_string(inline179)
                t148 = inline180
                var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
                _goml_runtime_core_string_println(inline176)
                return struct{}{}
            case false:
                var t150 string
                var inline185 int = 3
                var inline186 string = _goml_runtime_core_int_to_string(inline185)
                t150 = inline186
                var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
                _goml_runtime_core_string_println(inline182)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x136 {
            case true:
                var t153 string
                var inline191 int = 4
                var inline192 string = _goml_runtime_core_int_to_string(inline191)
                t153 = inline192
                var inline188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t153)
                _goml_runtime_core_string_println(inline188)
                return struct{}{}
            case false:
                var t155 string
                var inline197 int = 2
                var inline198 string = _goml_runtime_core_int_to_string(inline197)
                t155 = inline198
                var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
                _goml_runtime_core_string_println(inline194)
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
    var t158 T = B{
        _0: true,
        _1: true,
    }
    test(t158)
    var t159 T = B{
        _0: false,
        _1: true,
    }
    test(t159)
    var t160 T = B{
        _0: false,
        _1: false,
    }
    test(t160)
    test(A{})
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
