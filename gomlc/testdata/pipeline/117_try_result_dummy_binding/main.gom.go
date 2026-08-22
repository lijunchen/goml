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

type Result__unit__string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var mtmp796 Result__unit__string
    if config_ok__3 {
        var inline856 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        mtmp796 = inline856
    } else {
        var inline857 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: "config failed",
        }
        mtmp796 = inline857
    }
    switch mtmp796._tag {
    case 0:
        var mtmp800 Result__string__string
        if read_ok__4 {
            var inline853 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: "2s",
            }
            mtmp800 = inline853
        } else {
            var inline854 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: "duration failed",
            }
            mtmp800 = inline854
        }
        var jp827 string
        switch mtmp800._tag {
        case 0:
            var x801 string = mtmp800._v0_0
            jp827 = x801
            var t828 string
            var inline851 string = "duration=" + jp827
            t828 = inline851
            var t829 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t828,
            }
            return t829
        case 1:
            var x802 string = mtmp800._v1_0
            var t830 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x802,
            }
            return t830
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x798 string = mtmp796._v1_0
        var t831 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x798,
        }
        return t831
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t839 Result__string__string = configure_and_format(true, true)
    var t840 string
    switch t839._tag {
    case 0:
        var inline882 string = t839._v0_0
        var inline884 string = "ok " + inline882
        t840 = inline884
    case 1:
        var inline885 string = t839._v1_0
        var inline887 string = "err " + inline885
        t840 = inline887
    default:
        panic("non-exhaustive match")
    }
    var inline879 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t840)
    _goml_runtime_core_string_println(inline879)
    var t841 Result__string__string = configure_and_format(true, false)
    var t842 string
    switch t841._tag {
    case 0:
        var inline872 string = t841._v0_0
        var inline874 string = "ok " + inline872
        t842 = inline874
    case 1:
        var inline875 string = t841._v1_0
        var inline877 string = "err " + inline875
        t842 = inline877
    default:
        panic("non-exhaustive match")
    }
    var inline869 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t842)
    _goml_runtime_core_string_println(inline869)
    var t843 Result__string__string = configure_and_format(false, true)
    var t844 string
    switch t843._tag {
    case 0:
        var inline862 string = t843._v0_0
        var inline864 string = "ok " + inline862
        t844 = inline864
    case 1:
        var inline865 string = t843._v1_0
        var inline867 string = "err " + inline865
        t844 = inline867
    default:
        panic("non-exhaustive match")
    }
    var inline859 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t844)
    _goml_runtime_core_string_println(inline859)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
