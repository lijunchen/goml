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
    var t176 Option__int32
    var inline229 int32 = 3
    var inline230 bool = true
    var inline231 closure_env_run_0 = closure_env_run_0{
        flag_0: inline230,
        base_1: inline229,
    }
    var inline232 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline231)
    t176 = inline232
    var t177 string
    switch t176.(type) {
    case None:
        t177 = "none"
    case Some:
        var inline224 int32 = t176.(Some)._0
        var inline226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline224)
        var inline227 string = "some=" + inline226
        t177 = inline227
    default:
        panic("non-exhaustive match")
    }
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline221)
    var t178 Option__int32
    var inline216 int32 = 3
    var inline217 bool = false
    var inline218 closure_env_run_0 = closure_env_run_0{
        flag_0: inline217,
        base_1: inline216,
    }
    var inline219 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline218)
    t178 = inline219
    var t179 string
    switch t178.(type) {
    case None:
        t179 = "none"
    case Some:
        var inline211 int32 = t178.(Some)._0
        var inline213 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline211)
        var inline214 string = "some=" + inline213
        t179 = inline214
    default:
        panic("non-exhaustive match")
    }
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
    _goml_runtime_core_string_println(inline208)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t182 string = _goml_runtime_core_int32_to_string(self__6)
    return t182
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env160 closure_env_run_0) Option__int32 {
    var flag__2 bool = env160.flag_0
    var base__1 int32 = env160.base_1
    var mtmp155 Option__int32
    if flag__2 {
        var inline235 Option__int32 = Some{
            _0: 4,
        }
        mtmp155 = inline235
    } else {
        mtmp155 = None{}
    }
    var jp191 int32
    switch mtmp155.(type) {
    case None:
        return None{}
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        jp191 = x156
        var t192 int32 = jp191 + base__1
        var t193 Option__int32 = Some{
            _0: t192,
        }
        return t193
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
