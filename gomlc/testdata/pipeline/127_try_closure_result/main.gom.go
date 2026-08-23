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

type closure_env_run_0 struct {
    ok_0 bool
    prefix_1 string
}

type Ordering int32

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func main0() struct{} {
    var t0 Result__string__string
    var inline17 string = "outer"
    var inline18 bool = true
    var inline19 closure_env_run_0 = closure_env_run_0{
        ok_0: inline18,
        prefix_1: inline17,
    }
    var inline20 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline19)
    }
    var inline21 Result__string__string = inline20()
    t0 = inline21
    var t1 string
    switch t0._tag {
    case 0:
        var inline13 string = t0._v0_0
        var inline14_lhs string = "ok "
        var inline14 string = inline14_lhs + inline13
        t1 = inline14
    case 1:
        var inline15 string = t0._v1_0
        var inline16_lhs string = "err "
        var inline16 string = inline16_lhs + inline15
        t1 = inline16
    default:
        panic("non-exhaustive match")
    }
    var inline11 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1)
    _goml_runtime_core_string_println(inline11)
    var t2 Result__string__string
    var inline6 string = "outer"
    var inline7 bool = false
    var inline8 closure_env_run_0 = closure_env_run_0{
        ok_0: inline7,
        prefix_1: inline6,
    }
    var inline9 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline8)
    }
    var inline10 Result__string__string = inline9()
    t2 = inline10
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
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__0 string) string {
    return self__0
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env0 closure_env_run_0) Result__string__string {
    var ok__0 bool = env0.ok_0
    var prefix__0 string = env0.prefix_1
    var mtmp0 Result__string__string
    if ok__0 {
        var inline0 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "body",
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
        var t0_rhs string = ":"
        var t0 string = prefix__0 + t0_rhs
        var t1 string = t0 + jp0
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
}

func main() {
    main0()
}
