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

func split_host_port(ok__0 bool) _goml_m_Result_____o_string_c_string_q_____string {
    if ok__0 {
        var t810 Tuple2_6string_6string = Tuple2_6string_6string{
            _0: "localhost",
            _1: "8080",
        }
        var t811 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 0,
            _v0_0: t810,
        }
        return t811
    } else {
        var t812 _goml_m_Result_____o_string_c_string_q_____string = _goml_m_Result_____o_string_c_string_q_____string{
            _tag: 1,
            _v1_0: "missing port",
        }
        return t812
    }
}

func render(ok__2 bool) Result__string__string {
    var mtmp796 _goml_m_Result_____o_string_c_string_q_____string
    var inline845 _goml_m_Result_____o_string_c_string_q_____string = split_host_port(ok__2)
    mtmp796 = inline845
    var jp819 Tuple2_6string_6string
    switch mtmp796._tag {
    case 0:
        var x797 Tuple2_6string_6string = mtmp796._v0_0
        jp819 = x797
        var x800 string = jp819._0
        var x801 string = jp819._1
        var t820 string = x800 + "="
        var t821 string = t820 + x801
        var t822 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t821,
        }
        return t822
    case 1:
        var x798 string = mtmp796._v1_0
        var t823 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x798,
        }
        return t823
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t831 Result__string__string = render(true)
    var t832 string
    switch t831._tag {
    case 0:
        var inline860 string = t831._v0_0
        var inline862 string = "ok " + inline860
        t832 = inline862
    case 1:
        var inline863 string = t831._v1_0
        var inline865 string = "err " + inline863
        t832 = inline865
    default:
        panic("non-exhaustive match")
    }
    var inline857 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t832)
    _goml_runtime_core_string_println(inline857)
    var t833 Result__string__string = render(false)
    var t834 string
    switch t833._tag {
    case 0:
        var inline850 string = t833._v0_0
        var inline852 string = "ok " + inline850
        t834 = inline852
    case 1:
        var inline853 string = t833._v1_0
        var inline855 string = "err " + inline853
        t834 = inline855
    default:
        panic("non-exhaustive match")
    }
    var inline847 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t834)
    _goml_runtime_core_string_println(inline847)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
