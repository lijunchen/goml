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
    var t205 Result__int32__string
    var inline266 bool = true
    var inline267 closure_env_run_0 = closure_env_run_0{
        flag_0: inline266,
    }
    var inline268 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline267)
    t205 = inline268
    var t206 string
    switch t205.(type) {
    case Ok:
        var inline258 int32 = t205.(Ok)._0
        var inline260 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline258)
        var inline261 string = "ok=" + inline260
        t206 = inline261
    case Err:
        var inline262 string = t205.(Err)._0
        var inline264 string = "err=" + inline262
        t206 = inline264
    default:
        panic("non-exhaustive match")
    }
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline255)
    var t207 Result__int32__string
    var inline251 bool = false
    var inline252 closure_env_run_0 = closure_env_run_0{
        flag_0: inline251,
    }
    var inline253 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline252)
    t207 = inline253
    var t208 string
    switch t207.(type) {
    case Ok:
        var inline243 int32 = t207.(Ok)._0
        var inline245 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline243)
        var inline246 string = "ok=" + inline245
        t208 = inline246
    case Err:
        var inline247 string = t207.(Err)._0
        var inline249 string = "err=" + inline247
        t208 = inline249
    default:
        panic("non-exhaustive match")
    }
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline240)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t211 string = _goml_runtime_core_int32_to_string(self__35)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env184 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env184.flag_0
    var mtmp177 Result__int32__string
    if flag__3 {
        var inline274 Result__int32__string = Ok{
            _0: 7,
        }
        mtmp177 = inline274
    } else {
        var inline275 Result__int32__string = Err{
            _0: "nope",
        }
        mtmp177 = inline275
    }
    var jp220 int32
    switch mtmp177.(type) {
    case Ok:
        var x178 int32 = mtmp177.(Ok)._0
        jp220 = x178
        var t221 int32
        var inline271 int32 = 1
        var inline272 int32 = jp220 + inline271
        t221 = inline272
        var t222 Result__int32__string = Ok{
            _0: t221,
        }
        return t222
    case Err:
        var x179 string = mtmp177.(Err)._0
        var t223 Result__int32__string = Err{
            _0: x179,
        }
        return t223
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
