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

type T struct {
    _tag int32
    _v1_0 bool
    _v1_1 bool
}

func test(t__0 T) struct{} {
    switch t__0._tag {
    case 0:
        var t416 string
        var inline445 int = 1
        var inline446 string = _goml_runtime_core_int_to_string(inline445)
        t416 = inline446
        var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
        _goml_runtime_core_string_println(inline442)
        return struct{}{}
    case 1:
        var x408 bool = t__0._v1_0
        var x409 bool = t__0._v1_1
        switch x409 {
        case true:
            switch x408 {
            case true:
                var t420 string
                var inline451 int = 4
                var inline452 string = _goml_runtime_core_int_to_string(inline451)
                t420 = inline452
                var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
                _goml_runtime_core_string_println(inline448)
                return struct{}{}
            case false:
                var t422 string
                var inline457 int = 3
                var inline458 string = _goml_runtime_core_int_to_string(inline457)
                t422 = inline458
                var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
                _goml_runtime_core_string_println(inline454)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x408 {
            case true:
                var t425 string
                var inline463 int = 4
                var inline464 string = _goml_runtime_core_int_to_string(inline463)
                t425 = inline464
                var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
                _goml_runtime_core_string_println(inline460)
                return struct{}{}
            case false:
                var t427 string
                var inline469 int = 2
                var inline470 string = _goml_runtime_core_int_to_string(inline469)
                t427 = inline470
                var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
                _goml_runtime_core_string_println(inline466)
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
    var t430 T = T{
        _tag: 1,
        _v1_0: true,
        _v1_1: true,
    }
    test(t430)
    var t431 T = T{
        _tag: 1,
        _v1_0: false,
        _v1_1: true,
    }
    test(t431)
    var t432 T = T{
        _tag: 1,
        _v1_0: false,
        _v1_1: false,
    }
    test(t432)
    test(T{
        _tag: 0,
    })
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
