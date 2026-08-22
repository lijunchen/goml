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
    var t821 Result__string__string
    var inline865 string = "outer"
    var inline866 bool = true
    var inline867 closure_env_run_0 = closure_env_run_0{
        ok_0: inline866,
        prefix_1: inline865,
    }
    var inline868 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline867)
    }
    var inline869 Result__string__string = inline868()
    t821 = inline869
    var t822 string
    switch t821._tag {
    case 0:
        var inline858 string = t821._v0_0
        var inline860 string = "ok " + inline858
        t822 = inline860
    case 1:
        var inline861 string = t821._v1_0
        var inline863 string = "err " + inline861
        t822 = inline863
    default:
        panic("non-exhaustive match")
    }
    var inline855 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
    _goml_runtime_core_string_println(inline855)
    var t823 Result__string__string
    var inline849 string = "outer"
    var inline850 bool = false
    var inline851 closure_env_run_0 = closure_env_run_0{
        ok_0: inline850,
        prefix_1: inline849,
    }
    var inline852 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline851)
    }
    var inline853 Result__string__string = inline852()
    t823 = inline853
    var t824 string
    switch t823._tag {
    case 0:
        var inline842 string = t823._v0_0
        var inline844 string = "ok " + inline842
        t824 = inline844
    case 1:
        var inline845 string = t823._v1_0
        var inline847 string = "err " + inline845
        t824 = inline847
    default:
        panic("non-exhaustive match")
    }
    var inline839 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
    _goml_runtime_core_string_println(inline839)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env803 closure_env_run_0) Result__string__string {
    var ok__2 bool = env803.ok_0
    var prefix__1 string = env803.prefix_1
    var mtmp796 Result__string__string
    if ok__2 {
        var inline872 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "body",
        }
        mtmp796 = inline872
    } else {
        var inline873 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp796 = inline873
    }
    var jp833 string
    switch mtmp796._tag {
    case 0:
        var x797 string = mtmp796._v0_0
        jp833 = x797
        var t834 string = prefix__1 + ":"
        var t835 string = t834 + jp833
        var t836 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t835,
        }
        return t836
    case 1:
        var x798 string = mtmp796._v1_0
        var t837 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x798,
        }
        return t837
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
