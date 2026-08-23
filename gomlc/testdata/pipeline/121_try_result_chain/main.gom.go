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
        var t0 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "goml",
        }
        return t0
    } else {
        var t1 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        return t1
    }
}

func normalize_text(ok__0 bool) Result__string__string {
    var mtmp0 Result__string__string
    if ok__0 {
        var inline0 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "goml",
        }
        mtmp0 = inline0
    } else {
        var inline1 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp0 = inline1
    }
    var jp0 string
    switch mtmp0._tag {
    case 0:
        var x0 string = mtmp0._v0_0
        jp0 = x0
        var t0 string = jp0 + "!"
        var t1 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t0,
        }
        return t1
    case 1:
        var x1 string = mtmp0._v1_0
        var t2 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x1,
        }
        return t2
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__0 bool) Result__string__string {
    var mtmp0 Result__string__string
    var inline0 Result__string__string = parse_text(ok__0)
    var inline1 string
    switch inline0._tag {
    case 0:
        var inline4 string = inline0._v0_0
        inline1 = inline4
        var inline2 string = inline1 + "!"
        var inline3 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: inline2,
        }
        mtmp0 = inline3
        var jp0 string
        switch mtmp0._tag {
        case 0:
            var x0 string = mtmp0._v0_0
            jp0 = x0
            var t0 string = "[" + jp0
            var t1 string = t0 + "]"
            var t2 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t1,
            }
            return t2
        case 1:
            var x1 string = mtmp0._v1_0
            var t3 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x1,
            }
            return t3
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline5 string = inline0._v1_0
        var inline6 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline5,
        }
        mtmp0 = inline6
        var jp0 string
        switch mtmp0._tag {
        case 0:
            var x0 string = mtmp0._v0_0
            jp0 = x0
            var t0 string = "[" + jp0
            var t1 string = t0 + "]"
            var t2 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t1,
            }
            return t2
        case 1:
            var x1 string = mtmp0._v1_0
            var t3 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x1,
            }
            return t3
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t0 Result__string__string = decorate_text(true)
    var t1 string
    switch t0._tag {
    case 0:
        var inline17 string = t0._v0_0
        var inline18 string = "ok " + inline17
        t1 = inline18
    case 1:
        var inline19 string = t0._v1_0
        var inline20 string = "err " + inline19
        t1 = inline20
    default:
        panic("non-exhaustive match")
    }
    var inline15 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1)
    _goml_runtime_core_string_println(inline15)
    var t2 Result__string__string
    var inline6 bool = false
    var inline7 Result__string__string = normalize_text(inline6)
    var inline8 string
    switch inline7._tag {
    case 0:
        var inline12 string = inline7._v0_0
        inline8 = inline12
        var inline9 string = "[" + inline8
        var inline10 string = inline9 + "]"
        var inline11 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: inline10,
        }
        t2 = inline11
        var t3 string
        switch t2._tag {
        case 0:
            var inline2 string = t2._v0_0
            var inline3 string = "ok " + inline2
            t3 = inline3
        case 1:
            var inline4 string = t2._v1_0
            var inline5 string = "err " + inline4
            t3 = inline5
        default:
            panic("non-exhaustive match")
        }
        var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
        _goml_runtime_core_string_println(inline0)
        return struct{}{}
    case 1:
        var inline13 string = inline7._v1_0
        var inline14 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline13,
        }
        t2 = inline14
        var t3 string
        switch t2._tag {
        case 0:
            var inline2 string = t2._v0_0
            var inline3 string = "ok " + inline2
            t3 = inline3
        case 1:
            var inline4 string = t2._v1_0
            var inline5 string = "err " + inline4
            t3 = inline5
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
