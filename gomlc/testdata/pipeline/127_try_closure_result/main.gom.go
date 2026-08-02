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
    var t179 Result__string__string
    var inline222 string = "outer"
    var inline223 bool = true
    var inline224 closure_env_run_0 = closure_env_run_0{
        ok_0: inline223,
        prefix_1: inline222,
    }
    var inline225 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline224)
    t179 = inline225
    var t180 string
    switch t179.(type) {
    case Ok:
        var inline215 string = t179.(Ok)._0
        var inline217 string = "ok " + inline215
        t180 = inline217
    case Err:
        var inline218 string = t179.(Err)._0
        var inline220 string = "err " + inline218
        t180 = inline220
    default:
        panic("non-exhaustive match")
    }
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline212)
    var t181 Result__string__string
    var inline207 string = "outer"
    var inline208 bool = false
    var inline209 closure_env_run_0 = closure_env_run_0{
        ok_0: inline208,
        prefix_1: inline207,
    }
    var inline210 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline209)
    t181 = inline210
    var t182 string
    switch t181.(type) {
    case Ok:
        var inline200 string = t181.(Ok)._0
        var inline202 string = "ok " + inline200
        t182 = inline202
    case Err:
        var inline203 string = t181.(Err)._0
        var inline205 string = "err " + inline203
        t182 = inline205
    default:
        panic("non-exhaustive match")
    }
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env162 closure_env_run_0) Result__string__string {
    var ok__2 bool = env162.ok_0
    var prefix__1 string = env162.prefix_1
    var mtmp155 Result__string__string
    if ok__2 {
        var inline228 Result__string__string = Ok{
            _0: "body",
        }
        mtmp155 = inline228
    } else {
        var inline229 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp155 = inline229
    }
    var jp191 string
    switch mtmp155.(type) {
    case Ok:
        var x156 string = mtmp155.(Ok)._0
        jp191 = x156
        var t192 string = prefix__1 + ":"
        var t193 string = t192 + jp191
        var t194 Result__string__string = Ok{
            _0: t193,
        }
        return t194
    case Err:
        var x157 string = mtmp155.(Err)._0
        var t195 Result__string__string = Err{
            _0: x157,
        }
        return t195
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
