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
    var t197 Result__string__string
    var inline241 string = "outer"
    var inline242 bool = true
    var inline243 closure_env_run_0 = closure_env_run_0{
        ok_0: inline242,
        prefix_1: inline241,
    }
    var inline244 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline243)
    }
    var inline245 Result__string__string = inline244()
    t197 = inline245
    var t198 string
    switch t197.(type) {
    case Ok:
        var inline234 string = t197.(Ok)._0
        var inline236 string = "ok " + inline234
        t198 = inline236
    case Err:
        var inline237 string = t197.(Err)._0
        var inline239 string = "err " + inline237
        t198 = inline239
    default:
        panic("non-exhaustive match")
    }
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline231)
    var t199 Result__string__string
    var inline225 string = "outer"
    var inline226 bool = false
    var inline227 closure_env_run_0 = closure_env_run_0{
        ok_0: inline226,
        prefix_1: inline225,
    }
    var inline228 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline227)
    }
    var inline229 Result__string__string = inline228()
    t199 = inline229
    var t200 string
    switch t199.(type) {
    case Ok:
        var inline218 string = t199.(Ok)._0
        var inline220 string = "ok " + inline218
        t200 = inline220
    case Err:
        var inline221 string = t199.(Err)._0
        var inline223 string = "err " + inline221
        t200 = inline223
    default:
        panic("non-exhaustive match")
    }
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline215)
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
        var inline248 Result__string__string = Ok{
            _0: "body",
        }
        mtmp172 = inline248
    } else {
        var inline249 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp172 = inline249
    }
    var jp209 string
    switch mtmp172.(type) {
    case Ok:
        var x173 string = mtmp172.(Ok)._0
        jp209 = x173
        var t210 string = prefix__1 + ":"
        var t211 string = t210 + jp209
        var t212 Result__string__string = Ok{
            _0: t211,
        }
        return t212
    case Err:
        var x174 string = mtmp172.(Err)._0
        var t213 Result__string__string = Err{
            _0: x174,
        }
        return t213
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
