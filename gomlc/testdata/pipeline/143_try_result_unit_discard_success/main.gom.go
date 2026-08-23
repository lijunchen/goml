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
        var t0 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t0
    } else {
        var t1 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: "step failed",
        }
        return t1
    }
}

func main0() struct{} {
    var t0 Result__unit__string
    var inline15 bool = true
    var inline16 Result__unit__string = step(inline15)
    switch inline16._tag {
    case 0:
        var inline17 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        t0 = inline17
        var t1 string
        switch t0._tag {
        case 0:
            t1 = "ok unit"
        case 1:
            var inline13 string = t0._v1_0
            var inline14 string = "err " + inline13
            t1 = inline14
        default:
            panic("non-exhaustive match")
        }
        var inline10 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1)
        _goml_runtime_core_string_println(inline10)
        var t2 Result__unit__string
        var inline5 bool = false
        var inline6 Result__unit__string = step(inline5)
        switch inline6._tag {
        case 0:
            var inline7 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            t2 = inline7
            var t3 string
            switch t2._tag {
            case 0:
                t3 = "ok unit"
            case 1:
                var inline3 string = t2._v1_0
                var inline4 string = "err " + inline3
                t3 = inline4
            default:
                panic("non-exhaustive match")
            }
            var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
            _goml_runtime_core_string_println(inline0)
            return struct{}{}
        case 1:
            var inline8 string = inline6._v1_0
            var inline9 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline8,
            }
            t2 = inline9
            var t3 string
            switch t2._tag {
            case 0:
                t3 = "ok unit"
            case 1:
                var inline3 string = t2._v1_0
                var inline4 string = "err " + inline3
                t3 = inline4
            default:
                panic("non-exhaustive match")
            }
            var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
            _goml_runtime_core_string_println(inline0)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline18 string = inline16._v1_0
        var inline19 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: inline18,
        }
        t0 = inline19
        var t1 string
        switch t0._tag {
        case 0:
            t1 = "ok unit"
        case 1:
            var inline13 string = t0._v1_0
            var inline14 string = "err " + inline13
            t1 = inline14
        default:
            panic("non-exhaustive match")
        }
        var inline10 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1)
        _goml_runtime_core_string_println(inline10)
        var t2 Result__unit__string
        var inline5 bool = false
        var inline6 Result__unit__string = step(inline5)
        switch inline6._tag {
        case 0:
            var inline7 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            t2 = inline7
            var t3 string
            switch t2._tag {
            case 0:
                t3 = "ok unit"
            case 1:
                var inline3 string = t2._v1_0
                var inline4 string = "err " + inline3
                t3 = inline4
            default:
                panic("non-exhaustive match")
            }
            var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
            _goml_runtime_core_string_println(inline0)
            return struct{}{}
        case 1:
            var inline8 string = inline6._v1_0
            var inline9 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline8,
            }
            t2 = inline9
            var t3 string
            switch t2._tag {
            case 0:
                t3 = "ok unit"
            case 1:
                var inline3 string = t2._v1_0
                var inline4 string = "err " + inline3
                t3 = inline4
            default:
                panic("non-exhaustive match")
            }
            var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
            _goml_runtime_core_string_println(inline0)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
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
