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
    var t211 Result__int32__string
    var inline260 bool = true
    var inline261 closure_env_run_0 = closure_env_run_0{
        flag_0: inline260,
    }
    var inline262 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline261)
    }
    var inline263 Result__int32__string = inline262()
    t211 = inline263
    var t212 string
    switch t211.(type) {
    case Ok:
        var inline252 int32 = t211.(Ok)._0
        var inline254 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline252)
        var inline255 string = "ok=" + inline254
        t212 = inline255
    case Err:
        var inline256 string = t211.(Err)._0
        var inline258 string = "err=" + inline256
        t212 = inline258
    default:
        panic("non-exhaustive match")
    }
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline249)
    var t213 Result__int32__string
    var inline244 bool = false
    var inline245 closure_env_run_0 = closure_env_run_0{
        flag_0: inline244,
    }
    var inline246 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline245)
    }
    var inline247 Result__int32__string = inline246()
    t213 = inline247
    var t214 string
    switch t213.(type) {
    case Ok:
        var inline236 int32 = t213.(Ok)._0
        var inline238 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
        var inline239 string = "ok=" + inline238
        t214 = inline239
    case Err:
        var inline240 string = t213.(Err)._0
        var inline242 string = "err=" + inline240
        t214 = inline242
    default:
        panic("non-exhaustive match")
    }
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline233)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t217 string = _goml_runtime_core_int32_to_string(self__33)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env189 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env189.flag_0
    var mtmp182 Result__int32__string
    if flag__3 {
        var inline269 Result__int32__string = Ok{
            _0: 7,
        }
        mtmp182 = inline269
    } else {
        var inline270 Result__int32__string = Err{
            _0: "nope",
        }
        mtmp182 = inline270
    }
    var jp226 int32
    switch mtmp182.(type) {
    case Ok:
        var x183 int32 = mtmp182.(Ok)._0
        jp226 = x183
        var t227 int32
        var inline266 int32 = 1
        var inline267 int32 = jp226 + inline266
        t227 = inline267
        var t228 Result__int32__string = Ok{
            _0: t227,
        }
        return t228
    case Err:
        var x184 string = mtmp182.(Err)._0
        var t229 Result__int32__string = Err{
            _0: x184,
        }
        return t229
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
