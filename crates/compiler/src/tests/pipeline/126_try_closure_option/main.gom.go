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
    var retv29 Option__int32
    var jp31 Option__int32
    if flag__0 {
        var t32 Option__int32 = Some{
            _0: 4,
        }
        jp31 = t32
    } else {
        jp31 = None{}
    }
    retv29 = jp31
    return retv29
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv34 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t35 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv34 = t35
    return retv34
}

func show(opt__5 Option__int32) string {
    var retv37 string
    var jp39 string
    switch opt__5.(type) {
    case None:
        jp39 = "none"
    case Some:
        var x24 int32 = opt__5.(Some)._0
        var value__6 int32 = x24
        var t40 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t41 string = "some=" + t40
        jp39 = t41
    default:
        panic("non-exhaustive match")
    }
    retv37 = jp39
    return retv37
}

func main0() struct{} {
    var t43 Option__int32 = with_base(3, true)
    var t44 string = show(t43)
    println__T_string(t44)
    var t45 Option__int32 = with_base(3, false)
    var t46 string = show(t45)
    println__T_string(t46)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv48 string
    var t49 string = _goml_runtime_core_int32_to_string(self__2)
    retv48 = t49
    return retv48
}

func println__T_string(value__1 string) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t51)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env27 closure_env_run_0) Option__int32 {
    var retv56 Option__int32
    var flag__2 bool = env27.flag_0
    var base__1 int32 = env27.base_1
    var mtmp22 Option__int32 = maybe_value(flag__2)
    var jp58 int32
    switch mtmp22.(type) {
    case None:
        retv56 = None{}
        return retv56
    case Some:
        var x23 int32 = mtmp22.(Some)._0
        var try_value__11 int32 = x23
        jp58 = try_value__11
        var value__3 int32 = jp58
        var t59 int32 = value__3 + base__1
        var t60 Option__int32 = Some{
            _0: t59,
        }
        retv56 = t60
        return retv56
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
