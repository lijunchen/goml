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
    var x172 bool = x__0._0
    var x173 bool = x__0._1
    switch x173 {
    case true:
        var x174 bool = y__1._0
        var x175 bool = y__1._1
        switch x175 {
        case true:
            switch x174 {
            case true:
                var inline217 string = "case4"
                var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline217)
                _goml_runtime_core_string_println(inline218)
                return struct{}{}
            case false:
                var inline221 string = "case3"
                var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline221)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline225 string = "case4"
            var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline225)
            _goml_runtime_core_string_println(inline226)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x172 {
        case true:
            var x176 bool = y__1._0
            var x177 bool = y__1._1
            switch x177 {
            case true:
                switch x176 {
                case true:
                    var inline229 string = "case2"
                    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline229)
                    _goml_runtime_core_string_println(inline230)
                    return struct{}{}
                case false:
                    var inline233 string = "case1"
                    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline233)
                    _goml_runtime_core_string_println(inline234)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline237 string = "case2"
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline237)
                _goml_runtime_core_string_println(inline238)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x178 bool = y__1._0
            var x179 bool = y__1._1
            switch x179 {
            case true:
                switch x178 {
                case true:
                    var inline241 string = "case4"
                    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline241)
                    _goml_runtime_core_string_println(inline242)
                    return struct{}{}
                case false:
                    var inline245 string = "case3"
                    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline245)
                    _goml_runtime_core_string_println(inline246)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline249 string = "case4"
                var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline249)
                _goml_runtime_core_string_println(inline250)
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
    var t203 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t204 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t203, t204)
    var t205 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t206 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t205, t206)
    var t207 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t208 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t207, t208)
    var t209 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t210 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t209, t210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
