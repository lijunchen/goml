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

type Handle struct {
    name string
}

type Ordering int32

type Result__Handle__string struct {
    _tag int32
    _v0_0 Handle
    _v1_0 string
}

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

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var mtmp796 Result__Handle__string
    if open_ok__3 {
        var inline856 Handle = Handle{
            name: "config",
        }
        var inline857 Result__Handle__string = Result__Handle__string{
            _tag: 0,
            _v0_0: inline856,
        }
        mtmp796 = inline857
    } else {
        var inline858 Result__Handle__string = Result__Handle__string{
            _tag: 1,
            _v1_0: "open failed",
        }
        mtmp796 = inline858
    }
    var jp826 Handle
    switch mtmp796._tag {
    case 0:
        var x797 Handle = mtmp796._v0_0
        jp826 = x797
        var name__6 string = jp826.name
        var mtmp799 Result__unit__string
        if close_ok__4 {
            var inline851 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            mtmp799 = inline851
        } else {
            var inline852 string = jp826.name
            var inline853 string = "close failed for " + inline852
            var inline854 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline853,
            }
            mtmp799 = inline854
        }
        switch mtmp799._tag {
        case 0:
            var t828 string = "closed " + name__6
            var t829 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t828,
            }
            return t829
        case 1:
            var x801 string = mtmp799._v1_0
            var t830 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x801,
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
    var t839 Result__string__string = use_handle(true, true)
    var t840 string
    switch t839._tag {
    case 0:
        var inline883 string = t839._v0_0
        var inline885 string = "ok " + inline883
        t840 = inline885
    case 1:
        var inline886 string = t839._v1_0
        var inline888 string = "err " + inline886
        t840 = inline888
    default:
        panic("non-exhaustive match")
    }
    var inline880 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t840)
    _goml_runtime_core_string_println(inline880)
    var t841 Result__string__string = use_handle(false, true)
    var t842 string
    switch t841._tag {
    case 0:
        var inline873 string = t841._v0_0
        var inline875 string = "ok " + inline873
        t842 = inline875
    case 1:
        var inline876 string = t841._v1_0
        var inline878 string = "err " + inline876
        t842 = inline878
    default:
        panic("non-exhaustive match")
    }
    var inline870 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t842)
    _goml_runtime_core_string_println(inline870)
    var t843 Result__string__string = use_handle(true, false)
    var t844 string
    switch t843._tag {
    case 0:
        var inline863 string = t843._v0_0
        var inline865 string = "ok " + inline863
        t844 = inline865
    case 1:
        var inline866 string = t843._v1_0
        var inline868 string = "err " + inline866
        t844 = inline868
    default:
        panic("non-exhaustive match")
    }
    var inline860 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t844)
    _goml_runtime_core_string_println(inline860)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
