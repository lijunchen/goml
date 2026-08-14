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

func test_nested_match(x__0 Tuple2_4bool_4bool, y__1 Tuple2_4bool_4bool) struct{} {
    var x182 bool = x__0._0
    var x183 bool = x__0._1
    switch x183 {
    case true:
        var x184 bool = y__1._0
        var x185 bool = y__1._1
        switch x185 {
        case true:
            switch x184 {
            case true:
                var inline227 string = "case4"
                var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline227)
                _goml_runtime_core_string_println(inline228)
                return struct{}{}
            case false:
                var inline231 string = "case3"
                var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline231)
                _goml_runtime_core_string_println(inline232)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline235 string = "case4"
            var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline235)
            _goml_runtime_core_string_println(inline236)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x182 {
        case true:
            var x186 bool = y__1._0
            var x187 bool = y__1._1
            switch x187 {
            case true:
                switch x186 {
                case true:
                    var inline239 string = "case2"
                    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline239)
                    _goml_runtime_core_string_println(inline240)
                    return struct{}{}
                case false:
                    var inline243 string = "case1"
                    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline243)
                    _goml_runtime_core_string_println(inline244)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline247 string = "case2"
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline247)
                _goml_runtime_core_string_println(inline248)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x188 bool = y__1._0
            var x189 bool = y__1._1
            switch x189 {
            case true:
                switch x188 {
                case true:
                    var inline251 string = "case4"
                    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline251)
                    _goml_runtime_core_string_println(inline252)
                    return struct{}{}
                case false:
                    var inline255 string = "case3"
                    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline255)
                    _goml_runtime_core_string_println(inline256)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline259 string = "case4"
                var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline259)
                _goml_runtime_core_string_println(inline260)
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
    var t213 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t214 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t213, t214)
    var t215 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t216 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t215, t216)
    var t217 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t218 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t217, t218)
    var t219 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t220 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t219, t220)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
