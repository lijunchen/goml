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
    var t201 Result__int32__string
    var inline250 bool = true
    var inline251 closure_env_run_0 = closure_env_run_0{
        flag_0: inline250,
    }
    var inline252 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline251)
    }
    var inline253 Result__int32__string = inline252()
    t201 = inline253
    var t202 string
    switch t201.(type) {
    case Ok:
        var inline242 int32 = t201.(Ok)._0
        var inline244 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline242)
        var inline245 string = "ok=" + inline244
        t202 = inline245
    case Err:
        var inline246 string = t201.(Err)._0
        var inline248 string = "err=" + inline246
        t202 = inline248
    default:
        panic("non-exhaustive match")
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline239)
    var t203 Result__int32__string
    var inline234 bool = false
    var inline235 closure_env_run_0 = closure_env_run_0{
        flag_0: inline234,
    }
    var inline236 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline235)
    }
    var inline237 Result__int32__string = inline236()
    t203 = inline237
    var t204 string
    switch t203.(type) {
    case Ok:
        var inline226 int32 = t203.(Ok)._0
        var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
        var inline229 string = "ok=" + inline228
        t204 = inline229
    case Err:
        var inline230 string = t203.(Err)._0
        var inline232 string = "err=" + inline230
        t204 = inline232
    default:
        panic("non-exhaustive match")
    }
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t207 string = _goml_runtime_core_int32_to_string(self__35)
    return t207
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env179 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env179.flag_0
    var mtmp172 Result__int32__string
    if flag__3 {
        var inline259 Result__int32__string = Ok{
            _0: 7,
        }
        mtmp172 = inline259
    } else {
        var inline260 Result__int32__string = Err{
            _0: "nope",
        }
        mtmp172 = inline260
    }
    var jp216 int32
    switch mtmp172.(type) {
    case Ok:
        var x173 int32 = mtmp172.(Ok)._0
        jp216 = x173
        var t217 int32
        var inline256 int32 = 1
        var inline257 int32 = jp216 + inline256
        t217 = inline257
        var t218 Result__int32__string = Ok{
            _0: t217,
        }
        return t218
    case Err:
        var x174 string = mtmp172.(Err)._0
        var t219 Result__int32__string = Err{
            _0: x174,
        }
        return t219
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
