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
    var t212 Result__string__string
    var inline256 string = "outer"
    var inline257 bool = true
    var inline258 closure_env_run_0 = closure_env_run_0{
        ok_0: inline257,
        prefix_1: inline256,
    }
    var inline259 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline258)
    }
    var inline260 Result__string__string = inline259()
    t212 = inline260
    var t213 string
    switch t212.(type) {
    case Ok:
        var inline249 string = t212.(Ok)._0
        var inline251 string = "ok " + inline249
        t213 = inline251
    case Err:
        var inline252 string = t212.(Err)._0
        var inline254 string = "err " + inline252
        t213 = inline254
    default:
        panic("non-exhaustive match")
    }
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline246)
    var t214 Result__string__string
    var inline240 string = "outer"
    var inline241 bool = false
    var inline242 closure_env_run_0 = closure_env_run_0{
        ok_0: inline241,
        prefix_1: inline240,
    }
    var inline243 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline242)
    }
    var inline244 Result__string__string = inline243()
    t214 = inline244
    var t215 string
    switch t214.(type) {
    case Ok:
        var inline233 string = t214.(Ok)._0
        var inline235 string = "ok " + inline233
        t215 = inline235
    case Err:
        var inline236 string = t214.(Err)._0
        var inline238 string = "err " + inline236
        t215 = inline238
    default:
        panic("non-exhaustive match")
    }
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline230)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env194 closure_env_run_0) Result__string__string {
    var ok__2 bool = env194.ok_0
    var prefix__1 string = env194.prefix_1
    var mtmp187 Result__string__string
    if ok__2 {
        var inline263 Result__string__string = Ok{
            _0: "body",
        }
        mtmp187 = inline263
    } else {
        var inline264 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp187 = inline264
    }
    var jp224 string
    switch mtmp187.(type) {
    case Ok:
        var x188 string = mtmp187.(Ok)._0
        jp224 = x188
        var t225 string = prefix__1 + ":"
        var t226 string = t225 + jp224
        var t227 Result__string__string = Ok{
            _0: t226,
        }
        return t227
    case Err:
        var x189 string = mtmp187.(Err)._0
        var t228 Result__string__string = Err{
            _0: x189,
        }
        return t228
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
