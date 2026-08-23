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

func use_handle(open_ok__0 bool, close_ok__0 bool) Result__string__string {
    var mtmp0 Result__Handle__string
    if open_ok__0 {
        var inline4 Handle = Handle{
            name: "config",
        }
        var inline5 Result__Handle__string = Result__Handle__string{
            _tag: 0,
            _v0_0: inline4,
        }
        mtmp0 = inline5
    } else {
        var inline6 Result__Handle__string = Result__Handle__string{
            _tag: 1,
            _v1_0: "open failed",
        }
        mtmp0 = inline6
    }
    var jp0 Handle
    switch mtmp0._tag {
    case 0:
        var x1 Handle = mtmp0._v0_0
        jp0 = x1
        var name__0 string = jp0.name
        var mtmp1 Result__unit__string
        if close_ok__0 {
            var inline0 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            mtmp1 = inline0
        } else {
            var inline1 string = jp0.name
            var inline2_lhs string = "close failed for "
            var inline2 string = inline2_lhs + inline1
            var inline3 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline2,
            }
            mtmp1 = inline3
        }
        switch mtmp1._tag {
        case 0:
            var t0_lhs string = "closed "
            var t0 string = t0_lhs + name__0
            var t1 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t0,
            }
            return t1
        case 1:
            var x0 string = mtmp1._v1_0
            var t2 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x0,
            }
            return t2
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x2 string = mtmp0._v1_0
        var t3 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x2,
        }
        return t3
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t0 Result__string__string = use_handle(true, true)
    var t1 string
    switch t0._tag {
    case 0:
        var inline14 string = t0._v0_0
        var inline15_lhs string = "ok "
        var inline15 string = inline15_lhs + inline14
        t1 = inline15
    case 1:
        var inline16 string = t0._v1_0
        var inline17_lhs string = "err "
        var inline17 string = inline17_lhs + inline16
        t1 = inline17
    default:
        panic("non-exhaustive match")
    }
    var inline12 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1)
    _goml_runtime_core_string_println(inline12)
    var t2 Result__string__string = use_handle(false, true)
    var t3 string
    switch t2._tag {
    case 0:
        var inline8 string = t2._v0_0
        var inline9_lhs string = "ok "
        var inline9 string = inline9_lhs + inline8
        t3 = inline9
    case 1:
        var inline10 string = t2._v1_0
        var inline11_lhs string = "err "
        var inline11 string = inline11_lhs + inline10
        t3 = inline11
    default:
        panic("non-exhaustive match")
    }
    var inline6 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t3)
    _goml_runtime_core_string_println(inline6)
    var t4 Result__string__string = use_handle(true, false)
    var t5 string
    switch t4._tag {
    case 0:
        var inline2 string = t4._v0_0
        var inline3_lhs string = "ok "
        var inline3 string = inline3_lhs + inline2
        t5 = inline3
    case 1:
        var inline4 string = t4._v1_0
        var inline5_lhs string = "err "
        var inline5 string = inline5_lhs + inline4
        t5 = inline5
    default:
        panic("non-exhaustive match")
    }
    var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t5)
    _goml_runtime_core_string_println(inline0)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__0 string) string {
    return self__0
}

func main() {
    main0()
}
