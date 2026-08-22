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

type _goml_m_Result_____o_string_c_string_q_____string struct {
    _tag int32
    _v0_0 Tuple2_6string_6string
    _v1_0 string
}

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func render(ok__1 bool) Result__string__string {
    var mtmp796 _goml_m_Result_____o_string_c_string_q_____string
    if ok__1 {
        var inline838 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "example.com",
            _1: "443",
        }
        var inline839 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 0,
            _v0_0: inline838,
        }
        mtmp796 = inline839
    } else {
        var inline840 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 1,
            _v1_0: "missing port",
        }
        mtmp796 = inline840
    }
    var jp816 Tuple2_6string_6string
    switch mtmp796._tag {
    case 0:
        var x797 Tuple2_6string_6string = mtmp796._v0_0
        jp816 = x797
        var x800 string = jp816._0
        var x801 string = jp816._1
        var t817 string = x800 + ":"
        var t818 string = t817 + x801
        var t819 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t818,
        }
        return t819
    case 1:
        var x798 string = mtmp796._v1_0
        var t820 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x798,
        }
        return t820
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t828 Result__string__string = render(true)
    var t829 string
    switch t828._tag {
    case 0:
        var inline855 string = t828._v0_0
        var inline857 string = "ok " + inline855
        t829 = inline857
    case 1:
        var inline858 string = t828._v1_0
        var inline860 string = "err " + inline858
        t829 = inline860
    default:
        panic("non-exhaustive match")
    }
    var inline852 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t829)
    _goml_runtime_core_string_println(inline852)
    var t830 Result__string__string = render(false)
    var t831 string
    switch t830._tag {
    case 0:
        var inline845 string = t830._v0_0
        var inline847 string = "ok " + inline845
        t831 = inline847
    case 1:
        var inline848 string = t830._v1_0
        var inline850 string = "err " + inline848
        t831 = inline850
    default:
        panic("non-exhaustive match")
    }
    var inline842 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t831)
    _goml_runtime_core_string_println(inline842)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
