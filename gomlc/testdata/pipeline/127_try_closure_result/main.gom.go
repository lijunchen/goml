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
    var t207 Result__string__string
    var inline251 string = "outer"
    var inline252 bool = true
    var inline253 closure_env_run_0 = closure_env_run_0{
        ok_0: inline252,
        prefix_1: inline251,
    }
    var inline254 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline253)
    }
    var inline255 Result__string__string = inline254()
    t207 = inline255
    var t208 string
    switch t207.(type) {
    case Ok:
        var inline244 string = t207.(Ok)._0
        var inline246 string = "ok " + inline244
        t208 = inline246
    case Err:
        var inline247 string = t207.(Err)._0
        var inline249 string = "err " + inline247
        t208 = inline249
    default:
        panic("non-exhaustive match")
    }
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline241)
    var t209 Result__string__string
    var inline235 string = "outer"
    var inline236 bool = false
    var inline237 closure_env_run_0 = closure_env_run_0{
        ok_0: inline236,
        prefix_1: inline235,
    }
    var inline238 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline237)
    }
    var inline239 Result__string__string = inline238()
    t209 = inline239
    var t210 string
    switch t209.(type) {
    case Ok:
        var inline228 string = t209.(Ok)._0
        var inline230 string = "ok " + inline228
        t210 = inline230
    case Err:
        var inline231 string = t209.(Err)._0
        var inline233 string = "err " + inline231
        t210 = inline233
    default:
        panic("non-exhaustive match")
    }
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline225)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env189 closure_env_run_0) Result__string__string {
    var ok__2 bool = env189.ok_0
    var prefix__1 string = env189.prefix_1
    var mtmp182 Result__string__string
    if ok__2 {
        var inline258 Result__string__string = Ok{
            _0: "body",
        }
        mtmp182 = inline258
    } else {
        var inline259 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp182 = inline259
    }
    var jp219 string
    switch mtmp182.(type) {
    case Ok:
        var x183 string = mtmp182.(Ok)._0
        jp219 = x183
        var t220 string = prefix__1 + ":"
        var t221 string = t220 + jp219
        var t222 Result__string__string = Ok{
            _0: t221,
        }
        return t222
    case Err:
        var x184 string = mtmp182.(Err)._0
        var t223 Result__string__string = Err{
            _0: x184,
        }
        return t223
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
