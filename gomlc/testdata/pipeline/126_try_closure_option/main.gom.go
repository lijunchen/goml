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
    base_1 int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func main0() struct{} {
    var t209 Option__int32
    var inline252 int32 = 3
    var inline253 bool = true
    var inline254 closure_env_run_0 = closure_env_run_0{
        flag_0: inline253,
        base_1: inline252,
    }
    var inline255 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline254)
    }
    var inline256 Option__int32 = inline255()
    t209 = inline256
    var t210 string
    switch t209.(type) {
    case None:
        t210 = "none"
    case Some:
        var inline247 int32 = t209.(Some)._0
        var inline249 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline247)
        var inline250 string = "some=" + inline249
        t210 = inline250
    default:
        panic("non-exhaustive match")
    }
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline244)
    var t211 Option__int32
    var inline238 int32 = 3
    var inline239 bool = false
    var inline240 closure_env_run_0 = closure_env_run_0{
        flag_0: inline239,
        base_1: inline238,
    }
    var inline241 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline240)
    }
    var inline242 Option__int32 = inline241()
    t211 = inline242
    var t212 string
    switch t211.(type) {
    case None:
        t212 = "none"
    case Some:
        var inline233 int32 = t211.(Some)._0
        var inline235 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline233)
        var inline236 string = "some=" + inline235
        t212 = inline236
    default:
        panic("non-exhaustive match")
    }
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline230)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t215 string = _goml_runtime_core_int32_to_string(self__33)
    return t215
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env192 closure_env_run_0) Option__int32 {
    var flag__2 bool = env192.flag_0
    var base__1 int32 = env192.base_1
    var mtmp187 Option__int32
    if flag__2 {
        var inline259 Option__int32 = Some{
            _0: 4,
        }
        mtmp187 = inline259
    } else {
        mtmp187 = None{}
    }
    var jp224 int32
    switch mtmp187.(type) {
    case None:
        return None{}
    case Some:
        var x188 int32 = mtmp187.(Some)._0
        jp224 = x188
        var t225 int32 = jp224 + base__1
        var t226 Option__int32 = Some{
            _0: t225,
        }
        return t226
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
