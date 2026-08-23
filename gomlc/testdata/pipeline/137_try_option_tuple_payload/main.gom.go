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
        var t0 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var t1 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: t0,
        }
        return t1
    } else {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
}

func describe(ok__0 bool) Option__string {
    var mtmp0 _goml_m_Option_____o_string_c_string_q_
    if ok__0 {
        var inline0 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "alpha",
            _1: "beta",
        }
        var inline1 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
            _tag: 1,
            _v1_0: inline0,
        }
        mtmp0 = inline1
    } else {
        mtmp0 = _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    }
    var jp0 Tuple2_6string_6string
    switch mtmp0._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x2 Tuple2_6string_6string = mtmp0._v1_0
        jp0 = x2
        var x0 string = jp0._0
        var x1 string = jp0._1
        var t0_rhs string = "|"
        var t0 string = x0 + t0_rhs
        var t1 string = t0 + x1
        var t2 Option__string = Option__string{
            _tag: 1,
            _v1_0: t1,
        }
        return t2
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t0 Option__string = describe(true)
    var t1 string
    switch t0._tag {
    case 0:
        t1 = "none"
    case 1:
        var inline15 string = t0._v1_0
        var inline16_lhs string = "some "
        var inline16 string = inline16_lhs + inline15
        t1 = inline16
    default:
        panic("non-exhaustive match")
    }
    var inline13 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1)
    _goml_runtime_core_string_println(inline13)
    var t2 Option__string
    var inline4 bool = false
    var inline5 _goml_m_Option_____o_string_c_string_q_ = cut_pair(inline4)
    var inline6 Tuple2_6string_6string
    switch inline5._tag {
    case 0:
        t2 = Option__string{
            _tag: 0,
        }
        var t3 string
        switch t2._tag {
        case 0:
            t3 = "none"
        case 1:
            var inline2 string = t2._v1_0
            var inline3_lhs string = "some "
            var inline3 string = inline3_lhs + inline2
            t3 = inline3
        default:
            panic("non-exhaustive match")
        }
        var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
        _goml_runtime_core_string_println(inline0)
        return struct{}{}
    case 1:
        var inline12 Tuple2_6string_6string = inline5._v1_0
        inline6 = inline12
        var inline7 string = inline6._0
        var inline8 string = inline6._1
        var inline9_rhs string = "|"
        var inline9 string = inline7 + inline9_rhs
        var inline10 string = inline9 + inline8
        var inline11 Option__string = Option__string{
            _tag: 1,
            _v1_0: inline10,
        }
        t2 = inline11
        var t3 string
        switch t2._tag {
        case 0:
            t3 = "none"
        case 1:
            var inline2 string = t2._v1_0
            var inline3_lhs string = "some "
            var inline3 string = inline3_lhs + inline2
            t3 = inline3
        default:
            panic("non-exhaustive match")
        }
        var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
        _goml_runtime_core_string_println(inline0)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__0 string) string {
    return self__0
}

func main() {
    main0()
}
