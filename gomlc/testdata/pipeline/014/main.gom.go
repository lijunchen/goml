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
    var x136 bool = x__0._0
    var x137 bool = x__0._1
    switch x137 {
    case true:
        var x138 bool = y__1._0
        var x139 bool = y__1._1
        switch x139 {
        case true:
            switch x138 {
            case true:
                var inline181 string = "case4"
                var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline181)
                _goml_runtime_core_string_println(inline182)
                return struct{}{}
            case false:
                var inline185 string = "case3"
                var inline186 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline185)
                _goml_runtime_core_string_println(inline186)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline189 string = "case4"
            var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline189)
            _goml_runtime_core_string_println(inline190)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x136 {
        case true:
            var x140 bool = y__1._0
            var x141 bool = y__1._1
            switch x141 {
            case true:
                switch x140 {
                case true:
                    var inline193 string = "case2"
                    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline193)
                    _goml_runtime_core_string_println(inline194)
                    return struct{}{}
                case false:
                    var inline197 string = "case1"
                    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline197)
                    _goml_runtime_core_string_println(inline198)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline201 string = "case2"
                var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline201)
                _goml_runtime_core_string_println(inline202)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x142 bool = y__1._0
            var x143 bool = y__1._1
            switch x143 {
            case true:
                switch x142 {
                case true:
                    var inline205 string = "case4"
                    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline205)
                    _goml_runtime_core_string_println(inline206)
                    return struct{}{}
                case false:
                    var inline209 string = "case3"
                    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline209)
                    _goml_runtime_core_string_println(inline210)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline213 string = "case4"
                var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline213)
                _goml_runtime_core_string_println(inline214)
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
    var t167 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t168 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t167, t168)
    var t169 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t170 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t169, t170)
    var t171 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t172 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t171, t172)
    var t173 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t174 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t173, t174)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
