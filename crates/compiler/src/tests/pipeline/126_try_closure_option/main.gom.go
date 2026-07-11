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
    var retv11 Option__int32
    var jp13 Option__int32
    if flag__0 {
        var t14 Option__int32 = Some{
            _0: 4,
        }
        jp13 = t14
    } else {
        jp13 = None{}
    }
    retv11 = jp13
    return retv11
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv16 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t17 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv16 = t17
    return retv16
}

func show(opt__5 Option__int32) string {
    var retv19 string
    var jp21 string
    switch opt__5.(type) {
    case None:
        jp21 = "none"
    case Some:
        var x6 int32 = opt__5.(Some)._0
        var value__6 int32 = x6
        var t22 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t23 string = "some=" + t22
        jp21 = t23
    default:
        panic("non-exhaustive match")
    }
    retv19 = jp21
    return retv19
}

func main0() struct{} {
    var t25 Option__int32 = with_base(3, true)
    var t26 string = show(t25)
    println__T_string(t26)
    var t27 Option__int32 = with_base(3, false)
    var t28 string = show(t27)
    println__T_string(t28)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv30 string
    var t31 string = _goml_runtime_core_int32_to_string(self__2)
    retv30 = t31
    return retv30
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv36 string
    retv36 = self__9
    return retv36
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env9 closure_env_run_0) Option__int32 {
    var retv38 Option__int32
    var flag__2 bool = env9.flag_0
    var base__1 int32 = env9.base_1
    var mtmp4 Option__int32 = maybe_value(flag__2)
    var jp40 int32
    switch mtmp4.(type) {
    case None:
        retv38 = None{}
        return retv38
    case Some:
        var x5 int32 = mtmp4.(Some)._0
        var try_value__11 int32 = x5
        jp40 = try_value__11
        var value__3 int32 = jp40
        var t41 int32 = value__3 + base__1
        var t42 Option__int32 = Some{
            _0: t41,
        }
        retv38 = t42
        return retv38
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
