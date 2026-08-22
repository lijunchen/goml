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

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func parse_text(ok__0 bool) Result__string__string {
    if ok__0 {
        var t808 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ignored",
        }
        return t808
    } else {
        var t809 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        return t809
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp796 Result__string__string
    if ok__1 {
        var inline833 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ignored",
        }
        mtmp796 = inline833
    } else {
        var inline834 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp796 = inline834
    }
    switch mtmp796._tag {
    case 0:
        var t814 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ok",
        }
        return t814
    case 1:
        var x798 string = mtmp796._v1_0
        var t815 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x798,
        }
        return t815
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t823 Result__string__string = check(true)
    var t824 string
    switch t823._tag {
    case 0:
        var inline861 string = t823._v0_0
        var inline863 string = "ok " + inline861
        t824 = inline863
    case 1:
        var inline864 string = t823._v1_0
        var inline866 string = "err " + inline864
        t824 = inline866
    default:
        panic("non-exhaustive match")
    }
    var inline858 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
    _goml_runtime_core_string_println(inline858)
    var t825 Result__string__string
    var inline846 bool = false
    var inline847 Result__string__string = parse_text(inline846)
    switch inline847._tag {
    case 0:
        var inline851 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ok",
        }
        t825 = inline851
        var t826 string
        switch t825._tag {
        case 0:
            var inline839 string = t825._v0_0
            var inline841 string = "ok " + inline839
            t826 = inline841
        case 1:
            var inline842 string = t825._v1_0
            var inline844 string = "err " + inline842
            t826 = inline844
        default:
            panic("non-exhaustive match")
        }
        var inline836 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
        _goml_runtime_core_string_println(inline836)
        return struct{}{}
    case 1:
        var inline854 string = inline847._v1_0
        var inline856 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline854,
        }
        t825 = inline856
        var t826 string
        switch t825._tag {
        case 0:
            var inline839 string = t825._v0_0
            var inline841 string = "ok " + inline839
            t826 = inline841
        case 1:
            var inline842 string = t825._v1_0
            var inline844 string = "err " + inline842
            t826 = inline844
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
