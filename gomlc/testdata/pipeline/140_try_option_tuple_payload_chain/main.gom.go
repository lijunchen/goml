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
            _0: "left",
            _1: "right",
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

func pair(ok__1 bool) _goml_m_Option_____o_string_c_string_q_ {
    if ok__1 {
        var inline836 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "left",
            _1: "right",
        }
        var inline837 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: inline836,
        }
        return inline837
    } else {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
}

func describe(ok__2 bool) Option__string {
    var mtmp796 _goml_m_Option_____o_string_c_string_q_
    var inline839 _goml_m_Option_____o_string_c_string_q_ = cut_pair(ok__2)
    mtmp796 = inline839
    var jp816 Tuple2_6string_6string
    switch mtmp796._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x797 Tuple2_6string_6string = mtmp796._v1_0
        jp816 = x797
        var x799 string = jp816._0
        var x800 string = jp816._1
        var t817 string = x799 + ":"
        var t818 string = t817 + x800
        var t819 Option__string = Option__string{
            _tag: 1,
            _v1_0: t818,
        }
        return t819
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t826 Option__string = describe(true)
    var t827 string
    switch t826._tag {
    case 0:
        t827 = "none"
    case 1:
        var inline866 string = t826._v1_0
        var inline868 string = "some " + inline866
        t827 = inline868
    default:
        panic("non-exhaustive match")
    }
    var inline863 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t827)
    _goml_runtime_core_string_println(inline863)
    var t828 Option__string
    var inline848 bool = false
    var inline849 _goml_m_Option_____o_string_c_string_q_ = pair(inline848)
    var inline851 Tuple2_6string_6string
    switch inline849._tag {
    case 0:
        t828 = Option__string{
            _tag: 0,
        }
        var t829 string
        switch t828._tag {
        case 0:
            t829 = "none"
        case 1:
            var inline844 string = t828._v1_0
            var inline846 string = "some " + inline844
            t829 = inline846
        default:
            panic("non-exhaustive match")
        }
        var inline841 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t829)
        _goml_runtime_core_string_println(inline841)
        return struct{}{}
    case 1:
        var inline860 Tuple2_6string_6string = inline849._v1_0
        inline851 = inline860
        var inline853 string = inline851._0
        var inline854 string = inline851._1
        var inline857 string = inline853 + ":"
        var inline858 string = inline857 + inline854
        var inline859 Option__string = Option__string{
            _tag: 1,
            _v1_0: inline858,
        }
        t828 = inline859
        var t829 string
        switch t828._tag {
        case 0:
            t829 = "none"
        case 1:
            var inline844 string = t828._v1_0
            var inline846 string = "some " + inline844
            t829 = inline846
        default:
            panic("non-exhaustive match")
        }
        var inline841 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t829)
        _goml_runtime_core_string_println(inline841)
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
