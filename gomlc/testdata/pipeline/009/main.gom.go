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
        var t419 string
        var inline448 int = 1
        var inline449 string = _goml_runtime_core_int_to_string(inline448)
        t419 = inline449
        var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
        _goml_runtime_core_string_println(inline445)
        return struct{}{}
    case 1:
        var x411 bool = t__0._v1_0
        var x412 bool = t__0._v1_1
        switch x412 {
        case true:
            switch x411 {
            case true:
                var t423 string
                var inline454 int = 4
                var inline455 string = _goml_runtime_core_int_to_string(inline454)
                t423 = inline455
                var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
                _goml_runtime_core_string_println(inline451)
                return struct{}{}
            case false:
                var t425 string
                var inline460 int = 3
                var inline461 string = _goml_runtime_core_int_to_string(inline460)
                t425 = inline461
                var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
                _goml_runtime_core_string_println(inline457)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x411 {
            case true:
                var t428 string
                var inline466 int = 4
                var inline467 string = _goml_runtime_core_int_to_string(inline466)
                t428 = inline467
                var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
                _goml_runtime_core_string_println(inline463)
                return struct{}{}
            case false:
                var t430 string
                var inline472 int = 2
                var inline473 string = _goml_runtime_core_int_to_string(inline472)
                t430 = inline473
                var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
                _goml_runtime_core_string_println(inline469)
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
    var t433 T = T{
        _tag: 1,
        _v1_0: true,
        _v1_1: true,
    }
    test(t433)
    var t434 T = T{
        _tag: 1,
        _v1_0: false,
        _v1_1: true,
    }
    test(t434)
    var t435 T = T{
        _tag: 1,
        _v1_0: false,
        _v1_1: false,
    }
    test(t435)
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
