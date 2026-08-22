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

type Option__string struct {
    _tag int32
    _v1_0 string
}

func cut_prefix(case_id__0 int32) Option__string {
    var t805 bool = case_id__0 == 0
    if t805 {
        var t806 Option__string = Option__string{
            _tag: 1,
            _v1_0: "ml",
        }
        return t806
    } else {
        return Option__string{
            _tag: 0,
        }
    }
}

func main0() struct{} {
    var t819 Option__string
    var inline856 int32 = 0
    var inline857 Option__string = cut_prefix(inline856)
    var inline859 string
    switch inline857._tag {
    case 0:
        t819 = Option__string{
            _tag: 0,
        }
        var t820 string
        switch t819._tag {
        case 0:
            t820 = "none"
        case 1:
            var inline852 string = t819._v1_0
            var inline854 string = "some " + inline852
            t820 = inline854
        default:
            panic("non-exhaustive match")
        }
        var inline849 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
        _goml_runtime_core_string_println(inline849)
        var t821 Option__string
        var inline839 int32 = 1
        var inline840 Option__string = cut_prefix(inline839)
        var inline842 string
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
            var inline846 string = inline840._v1_0
            inline842 = inline846
            var inline844 string = inline842 + "!"
            var inline845 Option__string = Option__string{
                _tag: 1,
                _v1_0: inline844,
            }
            t821 = inline845
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
        var inline863 string = inline857._v1_0
        inline859 = inline863
        var inline861 string = inline859 + "!"
        var inline862 Option__string = Option__string{
            _tag: 1,
            _v1_0: inline861,
        }
        t819 = inline862
        var t820 string
        switch t819._tag {
        case 0:
            t820 = "none"
        case 1:
            var inline852 string = t819._v1_0
            var inline854 string = "some " + inline852
            t820 = inline854
        default:
            panic("non-exhaustive match")
        }
        var inline849 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
        _goml_runtime_core_string_println(inline849)
        var t821 Option__string
        var inline839 int32 = 1
        var inline840 Option__string = cut_prefix(inline839)
        var inline842 string
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
            var inline846 string = inline840._v1_0
            inline842 = inline846
            var inline844 string = inline842 + "!"
            var inline845 Option__string = Option__string{
                _tag: 1,
                _v1_0: inline844,
            }
            t821 = inline845
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
