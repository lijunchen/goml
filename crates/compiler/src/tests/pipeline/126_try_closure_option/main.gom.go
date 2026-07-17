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
    var retv65 Option__int32
    var jp67 Option__int32
    if flag__0 {
        var t68 Option__int32 = Some{
            _0: 4,
        }
        jp67 = t68
    } else {
        jp67 = None{}
    }
    retv65 = jp67
    return retv65
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv70 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t71 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv70 = t71
    return retv70
}

func show(opt__5 Option__int32) string {
    var retv73 string
    var jp75 string
    switch opt__5.(type) {
    case None:
        jp75 = "none"
    case Some:
        var x60 int32 = opt__5.(Some)._0
        var value__6 int32 = x60
        var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t77 string = "some=" + t76
        jp75 = t77
    default:
        panic("non-exhaustive match")
    }
    retv73 = jp75
    return retv73
}

func main0() struct{} {
    var t79 Option__int32 = with_base(3, true)
    var t80 string = show(t79)
    println__T_string(t80)
    var t81 Option__int32 = with_base(3, false)
    var t82 string = show(t81)
    println__T_string(t82)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__2)
    retv84 = t85
    return retv84
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv90 string
    retv90 = self__34
    return retv90
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env63 closure_env_run_0) Option__int32 {
    var retv92 Option__int32
    var flag__2 bool = env63.flag_0
    var base__1 int32 = env63.base_1
    var mtmp58 Option__int32 = maybe_value(flag__2)
    var jp94 int32
    switch mtmp58.(type) {
    case None:
        retv92 = None{}
        return retv92
    case Some:
        var x59 int32 = mtmp58.(Some)._0
        var try_value__11 int32 = x59
        jp94 = try_value__11
        var value__3 int32 = jp94
        var t95 int32 = value__3 + base__1
        var t96 Option__int32 = Some{
            _0: t95,
        }
        retv92 = t96
        return retv92
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
