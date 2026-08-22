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
        var t808 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t809 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: t808,
        }
        return t809
    } else {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
}

func describe(ok__1 bool) Option__string {
    var mtmp796 _goml_m_Option_____o_string_c_string_q_
    if ok__1 {
        var inline833 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline834 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: inline833,
        }
        mtmp796 = inline834
    } else {
        mtmp796 = _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
    var jp813 Tuple2_6string_6string
    switch mtmp796._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x797 Tuple2_6string_6string = mtmp796._v1_0
        jp813 = x797
        var x799 string = jp813._0
        var x800 string = jp813._1
        var t814 string = x799 + "|"
        var t815 string = t814 + x800
        var t816 Option__string = Option__string{
            _tag: 1,
            _v1_0: t815,
        }
        return t816
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t823 Option__string = describe(true)
    var t824 string
    switch t823._tag {
    case 0:
        t824 = "none"
    case 1:
        var inline861 string = t823._v1_0
        var inline863 string = "some " + inline861
        t824 = inline863
    default:
        panic("non-exhaustive match")
    }
    var inline858 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
    _goml_runtime_core_string_println(inline858)
    var t825 Option__string
    var inline843 bool = false
    var inline844 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline843)
    var inline846 Tuple2_6string_6string
    switch inline844._tag {
    case 0:
        t825 = Option__string{
            _tag: 0,
        }
        var t826 string
        switch t825._tag {
        case 0:
            t826 = "none"
        case 1:
            var inline839 string = t825._v1_0
            var inline841 string = "some " + inline839
            t826 = inline841
        default:
            panic("non-exhaustive match")
        }
        var inline836 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
        _goml_runtime_core_string_println(inline836)
        return struct{}{}
    case 1:
        var inline855 Tuple2_6string_6string = inline844._v1_0
        inline846 = inline855
        var inline848 string = inline846._0
        var inline849 string = inline846._1
        var inline852 string = inline848 + "|"
        var inline853 string = inline852 + inline849
        var inline854 Option__string = Option__string{
            _tag: 1,
            _v1_0: inline853,
        }
        t825 = inline854
        var t826 string
        switch t825._tag {
        case 0:
            t826 = "none"
        case 1:
            var inline839 string = t825._v1_0
            var inline841 string = "some " + inline839
            t826 = inline841
        default:
            panic("non-exhaustive match")
        }
        var inline836 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
        _goml_runtime_core_string_println(inline836)
        return struct{}{}
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
