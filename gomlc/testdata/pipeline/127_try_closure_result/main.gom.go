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
    var t201 Result__string__string
    var inline244 string = "outer"
    var inline245 bool = true
    var inline246 closure_env_run_0 = closure_env_run_0{
        ok_0: inline245,
        prefix_1: inline244,
    }
    var inline247 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline246)
    t201 = inline247
    var t202 string
    switch t201.(type) {
    case Ok:
        var inline237 string = t201.(Ok)._0
        var inline239 string = "ok " + inline237
        t202 = inline239
    case Err:
        var inline240 string = t201.(Err)._0
        var inline242 string = "err " + inline240
        t202 = inline242
    default:
        panic("non-exhaustive match")
    }
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline234)
    var t203 Result__string__string
    var inline229 string = "outer"
    var inline230 bool = false
    var inline231 closure_env_run_0 = closure_env_run_0{
        ok_0: inline230,
        prefix_1: inline229,
    }
    var inline232 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline231)
    t203 = inline232
    var t204 string
    switch t203.(type) {
    case Ok:
        var inline222 string = t203.(Ok)._0
        var inline224 string = "ok " + inline222
        t204 = inline224
    case Err:
        var inline225 string = t203.(Err)._0
        var inline227 string = "err " + inline225
        t204 = inline227
    default:
        panic("non-exhaustive match")
    }
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env184 closure_env_run_0) Result__string__string {
    var ok__2 bool = env184.ok_0
    var prefix__1 string = env184.prefix_1
    var mtmp177 Result__string__string
    if ok__2 {
        var inline250 Result__string__string = Ok{
            _0: "body",
        }
        mtmp177 = inline250
    } else {
        var inline251 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp177 = inline251
    }
    var jp213 string
    switch mtmp177.(type) {
    case Ok:
        var x178 string = mtmp177.(Ok)._0
        jp213 = x178
        var t214 string = prefix__1 + ":"
        var t215 string = t214 + jp213
        var t216 Result__string__string = Ok{
            _0: t215,
        }
        return t216
    case Err:
        var x179 string = mtmp177.(Err)._0
        var t217 Result__string__string = Err{
            _0: x179,
        }
        return t217
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
