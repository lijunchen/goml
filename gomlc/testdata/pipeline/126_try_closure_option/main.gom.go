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
    var t194 Option__int32
    var inline237 int32 = 3
    var inline238 bool = true
    var inline239 closure_env_run_0 = closure_env_run_0{
        flag_0: inline238,
        base_1: inline237,
    }
    var inline240 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline239)
    }
    var inline241 Option__int32 = inline240()
    t194 = inline241
    var t195 string
    switch t194.(type) {
    case None:
        t195 = "none"
    case Some:
        var inline232 int32 = t194.(Some)._0
        var inline234 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline232)
        var inline235 string = "some=" + inline234
        t195 = inline235
    default:
        panic("non-exhaustive match")
    }
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline229)
    var t196 Option__int32
    var inline223 int32 = 3
    var inline224 bool = false
    var inline225 closure_env_run_0 = closure_env_run_0{
        flag_0: inline224,
        base_1: inline223,
    }
    var inline226 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline225)
    }
    var inline227 Option__int32 = inline226()
    t196 = inline227
    var t197 string
    switch t196.(type) {
    case None:
        t197 = "none"
    case Some:
        var inline218 int32 = t196.(Some)._0
        var inline220 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline218)
        var inline221 string = "some=" + inline220
        t197 = inline221
    default:
        panic("non-exhaustive match")
    }
    var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline215)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t200 string = _goml_runtime_core_int32_to_string(self__33)
    return t200
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env177 closure_env_run_0) Option__int32 {
    var flag__2 bool = env177.flag_0
    var base__1 int32 = env177.base_1
    var mtmp172 Option__int32
    if flag__2 {
        var inline244 Option__int32 = Some{
            _0: 4,
        }
        mtmp172 = inline244
    } else {
        mtmp172 = None{}
    }
    var jp209 int32
    switch mtmp172.(type) {
    case None:
        return None{}
    case Some:
        var x173 int32 = mtmp172.(Some)._0
        jp209 = x173
        var t210 int32 = jp209 + base__1
        var t211 Option__int32 = Some{
            _0: t210,
        }
        return t211
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
