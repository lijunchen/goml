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
    var t157 Option__int32
    var inline210 int32 = 3
    var inline211 bool = true
    var inline212 closure_env_run_0 = closure_env_run_0{
        flag_0: inline211,
        base_1: inline210,
    }
    var inline213 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline212)
    t157 = inline213
    var t158 string
    switch t157.(type) {
    case None:
        t158 = "none"
    case Some:
        var inline205 int32 = t157.(Some)._0
        var inline207 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline205)
        var inline208 string = "some=" + inline207
        t158 = inline208
    default:
        panic("non-exhaustive match")
    }
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
    _goml_runtime_core_string_println(inline202)
    var t159 Option__int32
    var inline197 int32 = 3
    var inline198 bool = false
    var inline199 closure_env_run_0 = closure_env_run_0{
        flag_0: inline198,
        base_1: inline197,
    }
    var inline200 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline199)
    t159 = inline200
    var t160 string
    switch t159.(type) {
    case None:
        t160 = "none"
    case Some:
        var inline192 int32 = t159.(Some)._0
        var inline194 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline192)
        var inline195 string = "some=" + inline194
        t160 = inline195
    default:
        panic("non-exhaustive match")
    }
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline189)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t163 string = _goml_runtime_core_int32_to_string(self__35)
    return t163
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env141 closure_env_run_0) Option__int32 {
    var flag__2 bool = env141.flag_0
    var base__1 int32 = env141.base_1
    var mtmp136 Option__int32
    if flag__2 {
        var inline216 Option__int32 = Some{
            _0: 4,
        }
        mtmp136 = inline216
    } else {
        mtmp136 = None{}
    }
    var jp172 int32
    switch mtmp136.(type) {
    case None:
        return None{}
    case Some:
        var x137 int32 = mtmp136.(Some)._0
        jp172 = x137
        var t173 int32 = jp172 + base__1
        var t174 Option__int32 = Some{
            _0: t173,
        }
        return t174
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
