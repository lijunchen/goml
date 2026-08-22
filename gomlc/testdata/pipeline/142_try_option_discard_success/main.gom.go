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

type Tuple2_6string_6string struct {
    _0 string
    _1 string
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

type _goml_m_Option_____o_string_c_string_q_ struct {
    _tag int32
    _v1_0 Tuple2_6string_6string
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func cut_pair(ok__0 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__0 {
        var t806 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var t807 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: t806,
        }
        return t807
    } else {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
}

func main0() struct{} {
    var t819 Option__string
    var inline855 bool = true
    var inline856 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline855)
    switch inline856._tag {
    case 0:
        t819 = Option__string{
            _tag: 0,
        }
        var t820 string
        switch t819._tag {
        case 0:
            t820 = "none"
        case 1:
            var inline851 string = t819._v1_0
            var inline853 string = "some " + inline851
            t820 = inline853
        default:
            panic("non-exhaustive match")
        }
        var inline848 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
        _goml_runtime_core_string_println(inline848)
        var t821 Option__string
        var inline839 bool = false
        var inline840 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline839)
        switch inline840._tag {
        case 0:
            t821 = Option__string{
                _tag: 0,
            }
            var t822 string
            switch t821._tag {
            case 0:
                t822 = "none"
            case 1:
                var inline835 string = t821._v1_0
                var inline837 string = "some " + inline835
                t822 = inline837
            default:
                panic("non-exhaustive match")
            }
            var inline832 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
            _goml_runtime_core_string_println(inline832)
            return struct{}{}
        case 1:
            var inline844 Option__string = Option__string{
                _tag: 1,
                _v1_0: "ok",
            }
            t821 = inline844
            var t822 string
            switch t821._tag {
            case 0:
                t822 = "none"
            case 1:
                var inline835 string = t821._v1_0
                var inline837 string = "some " + inline835
                t822 = inline837
            default:
                panic("non-exhaustive match")
            }
            var inline832 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
            _goml_runtime_core_string_println(inline832)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline860 Option__string = Option__string{
            _tag: 1,
            _v1_0: "ok",
        }
        t819 = inline860
        var t820 string
        switch t819._tag {
        case 0:
            t820 = "none"
        case 1:
            var inline851 string = t819._v1_0
            var inline853 string = "some " + inline851
            t820 = inline853
        default:
            panic("non-exhaustive match")
        }
        var inline848 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
        _goml_runtime_core_string_println(inline848)
        var t821 Option__string
        var inline839 bool = false
        var inline840 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline839)
        switch inline840._tag {
        case 0:
            t821 = Option__string{
                _tag: 0,
            }
            var t822 string
            switch t821._tag {
            case 0:
                t822 = "none"
            case 1:
                var inline835 string = t821._v1_0
                var inline837 string = "some " + inline835
                t822 = inline837
            default:
                panic("non-exhaustive match")
            }
            var inline832 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
            _goml_runtime_core_string_println(inline832)
            return struct{}{}
        case 1:
            var inline844 Option__string = Option__string{
                _tag: 1,
                _v1_0: "ok",
            }
            t821 = inline844
            var t822 string
            switch t821._tag {
            case 0:
                t822 = "none"
            case 1:
                var inline835 string = t821._v1_0
                var inline837 string = "some " + inline835
                t822 = inline837
            default:
                panic("non-exhaustive match")
            }
            var inline832 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
            _goml_runtime_core_string_println(inline832)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
