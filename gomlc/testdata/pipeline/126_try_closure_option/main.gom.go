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
    var t193 Option__int32
    var inline246 int32 = 3
    var inline247 bool = true
    var inline248 closure_env_run_0 = closure_env_run_0{
        flag_0: inline247,
        base_1: inline246,
    }
    var inline249 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline248)
    t193 = inline249
    var t194 string
    switch t193.(type) {
    case None:
        t194 = "none"
    case Some:
        var inline241 int32 = t193.(Some)._0
        var inline243 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
        var inline244 string = "some=" + inline243
        t194 = inline244
    default:
        panic("non-exhaustive match")
    }
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline238)
    var t195 Option__int32
    var inline233 int32 = 3
    var inline234 bool = false
    var inline235 closure_env_run_0 = closure_env_run_0{
        flag_0: inline234,
        base_1: inline233,
    }
    var inline236 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline235)
    t195 = inline236
    var t196 string
    switch t195.(type) {
    case None:
        t196 = "none"
    case Some:
        var inline228 int32 = t195.(Some)._0
        var inline230 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline228)
        var inline231 string = "some=" + inline230
        t196 = inline231
    default:
        panic("non-exhaustive match")
    }
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline225)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t199 string = _goml_runtime_core_int32_to_string(self__35)
    return t199
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env177 closure_env_run_0) Option__int32 {
    var flag__2 bool = env177.flag_0
    var base__1 int32 = env177.base_1
    var mtmp172 Option__int32
    if flag__2 {
        var inline252 Option__int32 = Some{
            _0: 4,
        }
        mtmp172 = inline252
    } else {
        mtmp172 = None{}
    }
    var jp208 int32
    switch mtmp172.(type) {
    case None:
        return None{}
    case Some:
        var x173 int32 = mtmp172.(Some)._0
        jp208 = x173
        var t209 int32 = jp208 + base__1
        var t210 Option__int32 = Some{
            _0: t209,
        }
        return t210
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
