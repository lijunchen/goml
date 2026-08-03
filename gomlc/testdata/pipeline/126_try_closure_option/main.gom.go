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
    var t198 Option__int32
    var inline251 int32 = 3
    var inline252 bool = true
    var inline253 closure_env_run_0 = closure_env_run_0{
        flag_0: inline252,
        base_1: inline251,
    }
    var inline254 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline253)
    t198 = inline254
    var t199 string
    switch t198.(type) {
    case None:
        t199 = "none"
    case Some:
        var inline246 int32 = t198.(Some)._0
        var inline248 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline246)
        var inline249 string = "some=" + inline248
        t199 = inline249
    default:
        panic("non-exhaustive match")
    }
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline243)
    var t200 Option__int32
    var inline238 int32 = 3
    var inline239 bool = false
    var inline240 closure_env_run_0 = closure_env_run_0{
        flag_0: inline239,
        base_1: inline238,
    }
    var inline241 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline240)
    t200 = inline241
    var t201 string
    switch t200.(type) {
    case None:
        t201 = "none"
    case Some:
        var inline233 int32 = t200.(Some)._0
        var inline235 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline233)
        var inline236 string = "some=" + inline235
        t201 = inline236
    default:
        panic("non-exhaustive match")
    }
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline230)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t204 string = _goml_runtime_core_int32_to_string(self__35)
    return t204
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env182 closure_env_run_0) Option__int32 {
    var flag__2 bool = env182.flag_0
    var base__1 int32 = env182.base_1
    var mtmp177 Option__int32
    if flag__2 {
        var inline257 Option__int32 = Some{
            _0: 4,
        }
        mtmp177 = inline257
    } else {
        mtmp177 = None{}
    }
    var jp213 int32
    switch mtmp177.(type) {
    case None:
        return None{}
    case Some:
        var x178 int32 = mtmp177.(Some)._0
        jp213 = x178
        var t214 int32 = jp213 + base__1
        var t215 Option__int32 = Some{
            _0: t214,
        }
        return t215
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
