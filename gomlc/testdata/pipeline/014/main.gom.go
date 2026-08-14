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
    var x187 bool = x__0._0
    var x188 bool = x__0._1
    switch x188 {
    case true:
        var x189 bool = y__1._0
        var x190 bool = y__1._1
        switch x190 {
        case true:
            switch x189 {
            case true:
                var inline232 string = "case4"
                var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline232)
                _goml_runtime_core_string_println(inline233)
                return struct{}{}
            case false:
                var inline236 string = "case3"
                var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline236)
                _goml_runtime_core_string_println(inline237)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline240 string = "case4"
            var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline240)
            _goml_runtime_core_string_println(inline241)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x187 {
        case true:
            var x191 bool = y__1._0
            var x192 bool = y__1._1
            switch x192 {
            case true:
                switch x191 {
                case true:
                    var inline244 string = "case2"
                    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline244)
                    _goml_runtime_core_string_println(inline245)
                    return struct{}{}
                case false:
                    var inline248 string = "case1"
                    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline248)
                    _goml_runtime_core_string_println(inline249)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline252 string = "case2"
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline252)
                _goml_runtime_core_string_println(inline253)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x193 bool = y__1._0
            var x194 bool = y__1._1
            switch x194 {
            case true:
                switch x193 {
                case true:
                    var inline256 string = "case4"
                    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline256)
                    _goml_runtime_core_string_println(inline257)
                    return struct{}{}
                case false:
                    var inline260 string = "case3"
                    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline260)
                    _goml_runtime_core_string_println(inline261)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline264 string = "case4"
                var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline264)
                _goml_runtime_core_string_println(inline265)
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
    var t218 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t219 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t218, t219)
    var t220 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t221 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t220, t221)
    var t222 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t223 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t222, t223)
    var t224 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t225 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t224, t225)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
