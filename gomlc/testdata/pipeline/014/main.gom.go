package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint32 struct {
    items []uint32
}

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type Ordering int32

func test_nested_match(x__0 Tuple2_4bool_4bool, y__1 Tuple2_4bool_4bool) struct{} {
    var x796 bool = x__0._0
    var x797 bool = x__0._1
    switch x797 {
    case true:
        var x798 bool = y__1._0
        var x799 bool = y__1._1
        switch x799 {
        case true:
            switch x798 {
            case true:
                var inline841 string = "case4"
                var inline842 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline841)
                _goml_runtime_core_string_println(inline842)
                return struct{}{}
            case false:
                var inline845 string = "case3"
                var inline846 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline845)
                _goml_runtime_core_string_println(inline846)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var inline849 string = "case4"
            var inline850 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline849)
            _goml_runtime_core_string_println(inline850)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x796 {
        case true:
            var x800 bool = y__1._0
            var x801 bool = y__1._1
            switch x801 {
            case true:
                switch x800 {
                case true:
                    var inline853 string = "case2"
                    var inline854 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline853)
                    _goml_runtime_core_string_println(inline854)
                    return struct{}{}
                case false:
                    var inline857 string = "case1"
                    var inline858 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline857)
                    _goml_runtime_core_string_println(inline858)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline861 string = "case2"
                var inline862 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline861)
                _goml_runtime_core_string_println(inline862)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x802 bool = y__1._0
            var x803 bool = y__1._1
            switch x803 {
            case true:
                switch x802 {
                case true:
                    var inline865 string = "case4"
                    var inline866 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline865)
                    _goml_runtime_core_string_println(inline866)
                    return struct{}{}
                case false:
                    var inline869 string = "case3"
                    var inline870 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline869)
                    _goml_runtime_core_string_println(inline870)
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                var inline873 string = "case4"
                var inline874 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline873)
                _goml_runtime_core_string_println(inline874)
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
    var t827 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t828 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t827, t828)
    var t829 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t830 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t829, t830)
    var t831 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t832 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t831, t832)
    var t833 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t834 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t833, t834)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
