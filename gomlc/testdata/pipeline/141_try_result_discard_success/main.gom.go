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
            _v0_0: "ignored",
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

func check(ok__0 bool) Result__string__string {
    var mtmp0 Result__string__string
    if ok__0 {
        var inline0 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ignored",
        }
        mtmp0 = inline0
    } else {
        var inline1 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp0 = inline1
    }
    switch mtmp0._tag {
    case 0:
        var t0 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ok",
        }
        return t0
    case 1:
        var x1 string = mtmp0._v1_0
        var t1 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x1,
        }
        return t1
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t0 Result__string__string = check(true)
    var t1 string
    switch t0._tag {
    case 0:
        var inline15 string = t0._v0_0
        var inline16_lhs string = "ok "
        var inline16 string = inline16_lhs + inline15
        t1 = inline16
    case 1:
        var inline17 string = t0._v1_0
        var inline18_lhs string = "err "
        var inline18 string = inline18_lhs + inline17
        t1 = inline18
    default:
        panic("non-exhaustive match")
    }
    var inline13 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1)
    _goml_runtime_core_string_println(inline13)
    var t2 Result__string__string
    var inline6 bool = false
    var inline7 Result__string__string = parse_text(inline6)
    switch inline7._tag {
    case 0:
        var inline9 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ok",
        }
        t2 = inline9
        var t3 string
        switch t2._tag {
        case 0:
            var inline2 string = t2._v0_0
            var inline3_lhs string = "ok "
            var inline3 string = inline3_lhs + inline2
            t3 = inline3
        case 1:
            var inline4 string = t2._v1_0
            var inline5_lhs string = "err "
            var inline5 string = inline5_lhs + inline4
            t3 = inline5
        default:
            panic("non-exhaustive match")
        }
        var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
        _goml_runtime_core_string_println(inline0)
        return struct{}{}
    case 1:
        var inline11 string = inline7._v1_0
        var inline12 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline11,
        }
        t2 = inline12
        var t3 string
        switch t2._tag {
        case 0:
            var inline2 string = t2._v0_0
            var inline3_lhs string = "ok "
            var inline3 string = inline3_lhs + inline2
            t3 = inline3
        case 1:
            var inline4 string = t2._v1_0
            var inline5_lhs string = "err "
            var inline5 string = inline5_lhs + inline4
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
