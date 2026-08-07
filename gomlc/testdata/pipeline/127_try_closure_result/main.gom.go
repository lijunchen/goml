package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_run_0 struct {
    ok_0 bool
    prefix_1 string
}

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func main0() struct{} {
    var t196 Result__string__string
    var inline239 string = "outer"
    var inline240 bool = true
    var inline241 closure_env_run_0 = closure_env_run_0{
        ok_0: inline240,
        prefix_1: inline239,
    }
    var inline242 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline241)
    t196 = inline242
    var t197 string
    switch t196.(type) {
    case Ok:
        var inline232 string = t196.(Ok)._0
        var inline234 string = "ok " + inline232
        t197 = inline234
    case Err:
        var inline235 string = t196.(Err)._0
        var inline237 string = "err " + inline235
        t197 = inline237
    default:
        panic("non-exhaustive match")
    }
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline229)
    var t198 Result__string__string
    var inline224 string = "outer"
    var inline225 bool = false
    var inline226 closure_env_run_0 = closure_env_run_0{
        ok_0: inline225,
        prefix_1: inline224,
    }
    var inline227 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline226)
    t198 = inline227
    var t199 string
    switch t198.(type) {
    case Ok:
        var inline217 string = t198.(Ok)._0
        var inline219 string = "ok " + inline217
        t199 = inline219
    case Err:
        var inline220 string = t198.(Err)._0
        var inline222 string = "err " + inline220
        t199 = inline222
    default:
        panic("non-exhaustive match")
    }
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env179 closure_env_run_0) Result__string__string {
    var ok__2 bool = env179.ok_0
    var prefix__1 string = env179.prefix_1
    var mtmp172 Result__string__string
    if ok__2 {
        var inline245 Result__string__string = Ok{
            _0: "body",
        }
        mtmp172 = inline245
    } else {
        var inline246 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp172 = inline246
    }
    var jp208 string
    switch mtmp172.(type) {
    case Ok:
        var x173 string = mtmp172.(Ok)._0
        jp208 = x173
        var t209 string = prefix__1 + ":"
        var t210 string = t209 + jp208
        var t211 Result__string__string = Ok{
            _0: t210,
        }
        return t211
    case Err:
        var x174 string = mtmp172.(Err)._0
        var t212 Result__string__string = Err{
            _0: x174,
        }
        return t212
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
