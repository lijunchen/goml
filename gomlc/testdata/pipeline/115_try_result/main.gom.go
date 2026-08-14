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
}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func main0() struct{} {
    var t216 Result__int32__string
    var inline265 bool = true
    var inline266 closure_env_run_0 = closure_env_run_0{
        flag_0: inline265,
    }
    var inline267 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline266)
    }
    var inline268 Result__int32__string = inline267()
    t216 = inline268
    var t217 string
    switch t216.(type) {
    case Ok:
        var inline257 int32 = t216.(Ok)._0
        var inline259 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline257)
        var inline260 string = "ok=" + inline259
        t217 = inline260
    case Err:
        var inline261 string = t216.(Err)._0
        var inline263 string = "err=" + inline261
        t217 = inline263
    default:
        panic("non-exhaustive match")
    }
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline254)
    var t218 Result__int32__string
    var inline249 bool = false
    var inline250 closure_env_run_0 = closure_env_run_0{
        flag_0: inline249,
    }
    var inline251 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline250)
    }
    var inline252 Result__int32__string = inline251()
    t218 = inline252
    var t219 string
    switch t218.(type) {
    case Ok:
        var inline241 int32 = t218.(Ok)._0
        var inline243 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
        var inline244 string = "ok=" + inline243
        t219 = inline244
    case Err:
        var inline245 string = t218.(Err)._0
        var inline247 string = "err=" + inline245
        t219 = inline247
    default:
        panic("non-exhaustive match")
    }
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline238)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t222 string = _goml_runtime_core_int32_to_string(self__33)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env194 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env194.flag_0
    var mtmp187 Result__int32__string
    if flag__3 {
        var inline274 Result__int32__string = Ok{
            _0: 7,
        }
        mtmp187 = inline274
    } else {
        var inline275 Result__int32__string = Err{
            _0: "nope",
        }
        mtmp187 = inline275
    }
    var jp231 int32
    switch mtmp187.(type) {
    case Ok:
        var x188 int32 = mtmp187.(Ok)._0
        jp231 = x188
        var t232 int32
        var inline271 int32 = 1
        var inline272 int32 = jp231 + inline271
        t232 = inline272
        var t233 Result__int32__string = Ok{
            _0: t232,
        }
        return t233
    case Err:
        var x189 string = mtmp187.(Err)._0
        var t234 Result__int32__string = Err{
            _0: x189,
        }
        return t234
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
