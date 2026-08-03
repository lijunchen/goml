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
    var t164 Result__int32__string
    var inline225 bool = true
    var inline226 closure_env_run_0 = closure_env_run_0{
        flag_0: inline225,
    }
    var inline227 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline226)
    t164 = inline227
    var t165 string
    switch t164.(type) {
    case Ok:
        var inline217 int32 = t164.(Ok)._0
        var inline219 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline217)
        var inline220 string = "ok=" + inline219
        t165 = inline220
    case Err:
        var inline221 string = t164.(Err)._0
        var inline223 string = "err=" + inline221
        t165 = inline223
    default:
        panic("non-exhaustive match")
    }
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline214)
    var t166 Result__int32__string
    var inline210 bool = false
    var inline211 closure_env_run_0 = closure_env_run_0{
        flag_0: inline210,
    }
    var inline212 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline211)
    t166 = inline212
    var t167 string
    switch t166.(type) {
    case Ok:
        var inline202 int32 = t166.(Ok)._0
        var inline204 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline202)
        var inline205 string = "ok=" + inline204
        t167 = inline205
    case Err:
        var inline206 string = t166.(Err)._0
        var inline208 string = "err=" + inline206
        t167 = inline208
    default:
        panic("non-exhaustive match")
    }
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t170 string = _goml_runtime_core_int32_to_string(self__35)
    return t170
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env143 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env143.flag_0
    var mtmp136 Result__int32__string
    if flag__3 {
        var inline233 Result__int32__string = Ok{
            _0: 7,
        }
        mtmp136 = inline233
    } else {
        var inline234 Result__int32__string = Err{
            _0: "nope",
        }
        mtmp136 = inline234
    }
    var jp179 int32
    switch mtmp136.(type) {
    case Ok:
        var x137 int32 = mtmp136.(Ok)._0
        jp179 = x137
        var t180 int32
        var inline230 int32 = 1
        var inline231 int32 = jp179 + inline230
        t180 = inline231
        var t181 Result__int32__string = Ok{
            _0: t180,
        }
        return t181
    case Err:
        var x138 string = mtmp136.(Err)._0
        var t182 Result__int32__string = Err{
            _0: x138,
        }
        return t182
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
