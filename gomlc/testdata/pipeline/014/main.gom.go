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
    var x155 bool = x__0._0
    var x156 bool = x__0._1
    switch x156 {
    case true:
        var x157 bool = y__1._0
        var x158 bool = y__1._1
        switch x158 {
        case true:
            switch x157 {
            case true:
                var inline200 string = "case4"
                var inline201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline200)
                _goml_runtime_core_string_println(inline201)
                return struct{}{}
            case false:
                var inline204 string = "case3"
                var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline204)
                _goml_runtime_core_string_println(inline205)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline208 string = "case4"
            var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline208)
            _goml_runtime_core_string_println(inline209)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x155 {
        case true:
            var x159 bool = y__1._0
            var x160 bool = y__1._1
            switch x160 {
            case true:
                switch x159 {
                case true:
                    var inline212 string = "case2"
                    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline212)
                    _goml_runtime_core_string_println(inline213)
                    return struct{}{}
                case false:
                    var inline216 string = "case1"
                    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline216)
                    _goml_runtime_core_string_println(inline217)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline220 string = "case2"
                var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline220)
                _goml_runtime_core_string_println(inline221)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x161 bool = y__1._0
            var x162 bool = y__1._1
            switch x162 {
            case true:
                switch x161 {
                case true:
                    var inline224 string = "case4"
                    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline224)
                    _goml_runtime_core_string_println(inline225)
                    return struct{}{}
                case false:
                    var inline228 string = "case3"
                    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline228)
                    _goml_runtime_core_string_println(inline229)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline232 string = "case4"
                var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline232)
                _goml_runtime_core_string_println(inline233)
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
    var t186 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t187 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t186, t187)
    var t188 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t189 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t188, t189)
    var t190 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t191 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t190, t191)
    var t192 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t193 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t192, t193)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
