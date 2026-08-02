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
    if flag__0 {
        var t165 Option__int32 = Some{
            _0: 4,
        }
        return t165
    } else {
        return None{}
    }
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t168 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    return t168
}

func show(opt__5 Option__int32) string {
    switch opt__5.(type) {
    case None:
        return "none"
    case Some:
        var x157 int32 = opt__5.(Some)._0
        var t173 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x157)
        var t174 string = "some=" + t173
        return t174
    default:
        panic("non-exhaustive match")
    }
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
    var t182 string = _goml_runtime_core_int32_to_string(self__6)
    return t182
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env160 closure_env_run_0) Option__int32 {
    var flag__2 bool = env160.flag_0
    var base__1 int32 = env160.base_1
    var mtmp155 Option__int32 = maybe_value(flag__2)
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
