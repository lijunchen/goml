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
    var retv14 Option__int32
    var jp16 Option__int32
    if flag__0 {
        var t17 Option__int32 = Some{
            _0: 4,
        }
        jp16 = t17
    } else {
        jp16 = None{}
    }
    retv14 = jp16
    return retv14
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv19 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t20 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv19 = t20
    return retv19
}

func show(opt__5 Option__int32) string {
    var retv22 string
    var jp24 string
    switch opt__5.(type) {
    case None:
        jp24 = "none"
    case Some:
        var x9 int32 = opt__5.(Some)._0
        var value__6 int32 = x9
        var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t26 string = "some=" + t25
        jp24 = t26
    default:
        panic("non-exhaustive match")
    }
    retv22 = jp24
    return retv22
}

func main0() struct{} {
    var t28 Option__int32 = with_base(3, true)
    var t29 string = show(t28)
    println__T_string(t29)
    var t30 Option__int32 = with_base(3, false)
    var t31 string = show(t30)
    println__T_string(t31)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv33 string
    var t34 string = _goml_runtime_core_int32_to_string(self__2)
    retv33 = t34
    return retv33
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env12 closure_env_run_0) Option__int32 {
    var retv41 Option__int32
    var flag__2 bool = env12.flag_0
    var base__1 int32 = env12.base_1
    var mtmp7 Option__int32 = maybe_value(flag__2)
    var jp43 int32
    switch mtmp7.(type) {
    case None:
        retv41 = None{}
        return retv41
    case Some:
        var x8 int32 = mtmp7.(Some)._0
        var try_value__11 int32 = x8
        jp43 = try_value__11
        var value__3 int32 = jp43
        var t44 int32 = value__3 + base__1
        var t45 Option__int32 = Some{
            _0: t44,
        }
        retv41 = t45
        return retv41
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
