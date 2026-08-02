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

func maybe_value(flag__0 bool) Option__int32 {
    var retv162 Option__int32
    var jp164 Option__int32
    if flag__0 {
        var t165 Option__int32 = Some{
            _0: 4,
        }
        jp164 = t165
    } else {
        jp164 = None{}
    }
    retv162 = jp164
    return retv162
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv167 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t168 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv167 = t168
    return retv167
}

func show(opt__5 Option__int32) string {
    var retv170 string
    var jp172 string
    switch opt__5.(type) {
    case None:
        jp172 = "none"
    case Some:
        var x157 int32 = opt__5.(Some)._0
        var value__6 int32 = x157
        var t173 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t174 string = "some=" + t173
        jp172 = t174
    default:
        panic("non-exhaustive match")
    }
    retv170 = jp172
    return retv170
}

func main0() struct{} {
    var t176 Option__int32 = with_base(3, true)
    var t177 string = show(t176)
    println__T_string(t177)
    var t178 Option__int32 = with_base(3, false)
    var t179 string = show(t178)
    println__T_string(t179)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv181 string
    var t182 string = _goml_runtime_core_int32_to_string(self__6)
    retv181 = t182
    return retv181
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv187 string
    retv187 = self__38
    return retv187
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env160 closure_env_run_0) Option__int32 {
    var retv189 Option__int32
    var flag__2 bool = env160.flag_0
    var base__1 int32 = env160.base_1
    var mtmp155 Option__int32 = maybe_value(flag__2)
    var jp191 int32
    switch mtmp155.(type) {
    case None:
        retv189 = None{}
        return retv189
    case Some:
        var x156 int32 = mtmp155.(Some)._0
        var try_value__11 int32 = x156
        jp191 = try_value__11
        var value__3 int32 = jp191
        var t192 int32 = value__3 + base__1
        var t193 Option__int32 = Some{
            _0: t192,
        }
        retv189 = t193
        return retv189
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
