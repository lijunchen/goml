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
    var x177 bool = x__0._0
    var x178 bool = x__0._1
    switch x178 {
    case true:
        var x179 bool = y__1._0
        var x180 bool = y__1._1
        switch x180 {
        case true:
            switch x179 {
            case true:
                var inline222 string = "case4"
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline222)
                _goml_runtime_core_string_println(inline223)
                return struct{}{}
            case false:
                var inline226 string = "case3"
                var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline226)
                _goml_runtime_core_string_println(inline227)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline230 string = "case4"
            var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline230)
            _goml_runtime_core_string_println(inline231)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x177 {
        case true:
            var x181 bool = y__1._0
            var x182 bool = y__1._1
            switch x182 {
            case true:
                switch x181 {
                case true:
                    var inline234 string = "case2"
                    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline234)
                    _goml_runtime_core_string_println(inline235)
                    return struct{}{}
                case false:
                    var inline238 string = "case1"
                    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline238)
                    _goml_runtime_core_string_println(inline239)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline242 string = "case2"
                var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline242)
                _goml_runtime_core_string_println(inline243)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x183 bool = y__1._0
            var x184 bool = y__1._1
            switch x184 {
            case true:
                switch x183 {
                case true:
                    var inline246 string = "case4"
                    var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline246)
                    _goml_runtime_core_string_println(inline247)
                    return struct{}{}
                case false:
                    var inline250 string = "case3"
                    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline250)
                    _goml_runtime_core_string_println(inline251)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline254 string = "case4"
                var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline254)
                _goml_runtime_core_string_println(inline255)
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
    var t208 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t209 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t208, t209)
    var t210 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t211 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t210, t211)
    var t212 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t213 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t212, t213)
    var t214 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t215 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t214, t215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
