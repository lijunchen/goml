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
        var t810 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "goml",
        }
        return t810
    } else {
        var t811 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        return t811
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp796 Result__string__string
    if ok__1 {
        var inline844 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "goml",
        }
        mtmp796 = inline844
    } else {
        var inline845 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp796 = inline845
    }
    var jp815 string
    switch mtmp796._tag {
    case 0:
        var x797 string = mtmp796._v0_0
        jp815 = x797
        var t816 string = jp815 + "!"
        var t817 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t816,
        }
        return t817
    case 1:
        var x798 string = mtmp796._v1_0
        var t818 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x798,
        }
        return t818
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp799 Result__string__string
    var inline847 Result__string__string = parse_text(ok__3)
    var inline849 string
    switch inline847._tag {
    case 0:
        var inline853 string = inline847._v0_0
        inline849 = inline853
        var inline851 string = inline849 + "!"
        var inline852 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: inline851,
        }
        mtmp799 = inline852
        var jp822 string
        switch mtmp799._tag {
        case 0:
            var x800 string = mtmp799._v0_0
            jp822 = x800
            var t823 string = "[" + jp822
            var t824 string = t823 + "]"
            var t825 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t824,
            }
            return t825
        case 1:
            var x801 string = mtmp799._v1_0
            var t826 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x801,
            }
            return t826
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline855 string = inline847._v1_0
        var inline857 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline855,
        }
        mtmp799 = inline857
        var jp822 string
        switch mtmp799._tag {
        case 0:
            var x800 string = mtmp799._v0_0
            jp822 = x800
            var t823 string = "[" + jp822
            var t824 string = t823 + "]"
            var t825 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t824,
            }
            return t825
        case 1:
            var x801 string = mtmp799._v1_0
            var t826 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x801,
            }
            return t826
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t834 Result__string__string = decorate_text(true)
    var t835 string
    switch t834._tag {
    case 0:
        var inline886 string = t834._v0_0
        var inline888 string = "ok " + inline886
        t835 = inline888
    case 1:
        var inline889 string = t834._v1_0
        var inline891 string = "err " + inline889
        t835 = inline891
    default:
        panic("non-exhaustive match")
    }
    var inline883 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t835)
    _goml_runtime_core_string_println(inline883)
    var t836 Result__string__string
    var inline869 bool = false
    var inline870 Result__string__string = normalize_text(inline869)
    var inline872 string
    switch inline870._tag {
    case 0:
        var inline877 string = inline870._v0_0
        inline872 = inline877
        var inline874 string = "[" + inline872
        var inline875 string = inline874 + "]"
        var inline876 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: inline875,
        }
        t836 = inline876
        var t837 string
        switch t836._tag {
        case 0:
            var inline862 string = t836._v0_0
            var inline864 string = "ok " + inline862
            t837 = inline864
        case 1:
            var inline865 string = t836._v1_0
            var inline867 string = "err " + inline865
            t837 = inline867
        default:
            panic("non-exhaustive match")
        }
        var inline859 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t837)
        _goml_runtime_core_string_println(inline859)
        return struct{}{}
    case 1:
        var inline879 string = inline870._v1_0
        var inline881 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline879,
        }
        t836 = inline881
        var t837 string
        switch t836._tag {
        case 0:
            var inline862 string = t836._v0_0
            var inline864 string = "ok " + inline862
            t837 = inline864
        case 1:
            var inline865 string = t836._v1_0
            var inline867 string = "err " + inline865
            t837 = inline867
        default:
            panic("non-exhaustive match")
        }
        var inline859 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t837)
        _goml_runtime_core_string_println(inline859)
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
