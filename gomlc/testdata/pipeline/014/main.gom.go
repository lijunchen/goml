package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

type Ordering int32

func test_nested_match(x__0 Tuple2_4bool_4bool, y__1 Tuple2_4bool_4bool) struct{} {
    var x411 bool = x__0._0
    var x412 bool = x__0._1
    switch x412 {
    case true:
        var x413 bool = y__1._0
        var x414 bool = y__1._1
        switch x414 {
        case true:
            switch x413 {
            case true:
                var inline456 string = "case4"
                var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline456)
                _goml_runtime_core_string_println(inline457)
                return struct{}{}
            case false:
                var inline460 string = "case3"
                var inline461 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline460)
                _goml_runtime_core_string_println(inline461)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline464 string = "case4"
            var inline465 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline464)
            _goml_runtime_core_string_println(inline465)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x411 {
        case true:
            var x415 bool = y__1._0
            var x416 bool = y__1._1
            switch x416 {
            case true:
                switch x415 {
                case true:
                    var inline468 string = "case2"
                    var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline468)
                    _goml_runtime_core_string_println(inline469)
                    return struct{}{}
                case false:
                    var inline472 string = "case1"
                    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline472)
                    _goml_runtime_core_string_println(inline473)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline476 string = "case2"
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline476)
                _goml_runtime_core_string_println(inline477)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x417 bool = y__1._0
            var x418 bool = y__1._1
            switch x418 {
            case true:
                switch x417 {
                case true:
                    var inline480 string = "case4"
                    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline480)
                    _goml_runtime_core_string_println(inline481)
                    return struct{}{}
                case false:
                    var inline484 string = "case3"
                    var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline484)
                    _goml_runtime_core_string_println(inline485)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline488 string = "case4"
                var inline489 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline488)
                _goml_runtime_core_string_println(inline489)
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
    var t442 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t443 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t442, t443)
    var t444 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t445 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t444, t445)
    var t446 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t447 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t446, t447)
    var t448 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t449 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t448, t449)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
