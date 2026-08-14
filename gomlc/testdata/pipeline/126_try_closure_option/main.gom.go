package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_run_0 struct {
    flag_0 bool
    base_1 int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func main0() struct{} {
    var t204 Option__int32
    var inline247 int32 = 3
    var inline248 bool = true
    var inline249 closure_env_run_0 = closure_env_run_0{
        flag_0: inline248,
        base_1: inline247,
    }
    var inline250 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline249)
    }
    var inline251 Option__int32 = inline250()
    t204 = inline251
    var t205 string
    switch t204.(type) {
    case None:
        t205 = "none"
    case Some:
        var inline242 int32 = t204.(Some)._0
        var inline244 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline242)
        var inline245 string = "some=" + inline244
        t205 = inline245
    default:
        panic("non-exhaustive match")
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline239)
    var t206 Option__int32
    var inline233 int32 = 3
    var inline234 bool = false
    var inline235 closure_env_run_0 = closure_env_run_0{
        flag_0: inline234,
        base_1: inline233,
    }
    var inline236 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline235)
    }
    var inline237 Option__int32 = inline236()
    t206 = inline237
    var t207 string
    switch t206.(type) {
    case None:
        t207 = "none"
    case Some:
        var inline228 int32 = t206.(Some)._0
        var inline230 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline228)
        var inline231 string = "some=" + inline230
        t207 = inline231
    default:
        panic("non-exhaustive match")
    }
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline225)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t210 string = _goml_runtime_core_int32_to_string(self__33)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env187 closure_env_run_0) Option__int32 {
    var flag__2 bool = env187.flag_0
    var base__1 int32 = env187.base_1
    var mtmp182 Option__int32
    if flag__2 {
        var inline254 Option__int32 = Some{
            _0: 4,
        }
        mtmp182 = inline254
    } else {
        mtmp182 = None{}
    }
    var jp219 int32
    switch mtmp182.(type) {
    case None:
        return None{}
    case Some:
        var x183 int32 = mtmp182.(Some)._0
        jp219 = x183
        var t220 int32 = jp219 + base__1
        var t221 Option__int32 = Some{
            _0: t220,
        }
        return t221
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
