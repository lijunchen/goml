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
    var x408 bool = x__0._0
    var x409 bool = x__0._1
    switch x409 {
    case true:
        var x410 bool = y__1._0
        var x411 bool = y__1._1
        switch x411 {
        case true:
            switch x410 {
            case true:
                var inline453 string = "case4"
                var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline453)
                _goml_runtime_core_string_println(inline454)
                return struct{}{}
            case false:
                var inline457 string = "case3"
                var inline458 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline457)
                _goml_runtime_core_string_println(inline458)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline461 string = "case4"
            var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline461)
            _goml_runtime_core_string_println(inline462)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x408 {
        case true:
            var x412 bool = y__1._0
            var x413 bool = y__1._1
            switch x413 {
            case true:
                switch x412 {
                case true:
                    var inline465 string = "case2"
                    var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline465)
                    _goml_runtime_core_string_println(inline466)
                    return struct{}{}
                case false:
                    var inline469 string = "case1"
                    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline469)
                    _goml_runtime_core_string_println(inline470)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline473 string = "case2"
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline473)
                _goml_runtime_core_string_println(inline474)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x414 bool = y__1._0
            var x415 bool = y__1._1
            switch x415 {
            case true:
                switch x414 {
                case true:
                    var inline477 string = "case4"
                    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline477)
                    _goml_runtime_core_string_println(inline478)
                    return struct{}{}
                case false:
                    var inline481 string = "case3"
                    var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline481)
                    _goml_runtime_core_string_println(inline482)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline485 string = "case4"
                var inline486 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline485)
                _goml_runtime_core_string_println(inline486)
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
    var t439 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t440 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t439, t440)
    var t441 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t442 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t441, t442)
    var t443 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t444 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t443, t444)
    var t445 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t446 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t445, t446)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
