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

func step(ok__0 bool) Result__unit__string {
    if ok__0 {
        var t808 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t808
    } else {
        var t809 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: "step failed",
        }
        return t809
    }
}

func main0() struct{} {
    var t823 Result__unit__string
    var inline863 bool = true
    var inline864 Result__unit__string = step(inline863)
    switch inline864._tag {
    case 0:
        var inline867 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        t823 = inline867
        var t824 string
        switch t823._tag {
        case 0:
            t824 = "ok unit"
        case 1:
            var inline859 string = t823._v1_0
            var inline861 string = "err " + inline859
            t824 = inline861
        default:
            panic("non-exhaustive match")
        }
        var inline855 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
        _goml_runtime_core_string_println(inline855)
        var t825 Result__unit__string
        var inline844 bool = false
        var inline845 Result__unit__string = step(inline844)
        switch inline845._tag {
        case 0:
            var inline848 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            t825 = inline848
            var t826 string
            switch t825._tag {
            case 0:
                t826 = "ok unit"
            case 1:
                var inline840 string = t825._v1_0
                var inline842 string = "err " + inline840
                t826 = inline842
            default:
                panic("non-exhaustive match")
            }
            var inline836 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
            _goml_runtime_core_string_println(inline836)
            return struct{}{}
        case 1:
            var inline851 string = inline845._v1_0
            var inline853 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline851,
            }
            t825 = inline853
            var t826 string
            switch t825._tag {
            case 0:
                t826 = "ok unit"
            case 1:
                var inline840 string = t825._v1_0
                var inline842 string = "err " + inline840
                t826 = inline842
            default:
                panic("non-exhaustive match")
            }
            var inline836 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
            _goml_runtime_core_string_println(inline836)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline870 string = inline864._v1_0
        var inline872 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: inline870,
        }
        t823 = inline872
        var t824 string
        switch t823._tag {
        case 0:
            t824 = "ok unit"
        case 1:
            var inline859 string = t823._v1_0
            var inline861 string = "err " + inline859
            t824 = inline861
        default:
            panic("non-exhaustive match")
        }
        var inline855 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
        _goml_runtime_core_string_println(inline855)
        var t825 Result__unit__string
        var inline844 bool = false
        var inline845 Result__unit__string = step(inline844)
        switch inline845._tag {
        case 0:
            var inline848 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            t825 = inline848
            var t826 string
            switch t825._tag {
            case 0:
                t826 = "ok unit"
            case 1:
                var inline840 string = t825._v1_0
                var inline842 string = "err " + inline840
                t826 = inline842
            default:
                panic("non-exhaustive match")
            }
            var inline836 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
            _goml_runtime_core_string_println(inline836)
            return struct{}{}
        case 1:
            var inline851 string = inline845._v1_0
            var inline853 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline851,
            }
            t825 = inline853
            var t826 string
            switch t825._tag {
            case 0:
                t826 = "ok unit"
            case 1:
                var inline840 string = t825._v1_0
                var inline842 string = "err " + inline840
                t826 = inline842
            default:
                panic("non-exhaustive match")
            }
            var inline836 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
            _goml_runtime_core_string_println(inline836)
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
