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
    var t200 Result__int32__string
    var inline261 bool = true
    var inline262 closure_env_run_0 = closure_env_run_0{
        flag_0: inline261,
    }
    var inline263 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline262)
    t200 = inline263
    var t201 string
    switch t200.(type) {
    case Ok:
        var inline253 int32 = t200.(Ok)._0
        var inline255 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline253)
        var inline256 string = "ok=" + inline255
        t201 = inline256
    case Err:
        var inline257 string = t200.(Err)._0
        var inline259 string = "err=" + inline257
        t201 = inline259
    default:
        panic("non-exhaustive match")
    }
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline250)
    var t202 Result__int32__string
    var inline246 bool = false
    var inline247 closure_env_run_0 = closure_env_run_0{
        flag_0: inline246,
    }
    var inline248 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline247)
    t202 = inline248
    var t203 string
    switch t202.(type) {
    case Ok:
        var inline238 int32 = t202.(Ok)._0
        var inline240 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline238)
        var inline241 string = "ok=" + inline240
        t203 = inline241
    case Err:
        var inline242 string = t202.(Err)._0
        var inline244 string = "err=" + inline242
        t203 = inline244
    default:
        panic("non-exhaustive match")
    }
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline235)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t206 string = _goml_runtime_core_int32_to_string(self__35)
    return t206
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env179 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env179.flag_0
    var mtmp172 Result__int32__string
    if flag__3 {
        var inline269 Result__int32__string = Ok{
            _0: 7,
        }
        mtmp172 = inline269
    } else {
        var inline270 Result__int32__string = Err{
            _0: "nope",
        }
        mtmp172 = inline270
    }
    var jp215 int32
    switch mtmp172.(type) {
    case Ok:
        var x173 int32 = mtmp172.(Ok)._0
        jp215 = x173
        var t216 int32
        var inline266 int32 = 1
        var inline267 int32 = jp215 + inline266
        t216 = inline267
        var t217 Result__int32__string = Ok{
            _0: t216,
        }
        return t217
    case Err:
        var x174 string = mtmp172.(Err)._0
        var t218 Result__int32__string = Err{
            _0: x174,
        }
        return t218
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
