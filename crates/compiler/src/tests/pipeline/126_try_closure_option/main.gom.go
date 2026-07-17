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
    var retv68 Option__int32
    var jp70 Option__int32
    if flag__0 {
        var t71 Option__int32 = Some{
            _0: 4,
        }
        jp70 = t71
    } else {
        jp70 = None{}
    }
    retv68 = jp70
    return retv68
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv73 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t74 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv73 = t74
    return retv73
}

func show(opt__5 Option__int32) string {
    var retv76 string
    var jp78 string
    switch opt__5.(type) {
    case None:
        jp78 = "none"
    case Some:
        var x63 int32 = opt__5.(Some)._0
        var value__6 int32 = x63
        var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t80 string = "some=" + t79
        jp78 = t80
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func main0() struct{} {
    var t82 Option__int32 = with_base(3, true)
    var t83 string = show(t82)
    println__T_string(t83)
    var t84 Option__int32 = with_base(3, false)
    var t85 string = show(t84)
    println__T_string(t85)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__5)
    retv87 = t88
    return retv87
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv93 string
    retv93 = self__37
    return retv93
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env66 closure_env_run_0) Option__int32 {
    var retv95 Option__int32
    var flag__2 bool = env66.flag_0
    var base__1 int32 = env66.base_1
    var mtmp61 Option__int32 = maybe_value(flag__2)
    var jp97 int32
    switch mtmp61.(type) {
    case None:
        retv95 = None{}
        return retv95
    case Some:
        var x62 int32 = mtmp61.(Some)._0
        var try_value__11 int32 = x62
        jp97 = try_value__11
        var value__3 int32 = jp97
        var t98 int32 = value__3 + base__1
        var t99 Option__int32 = Some{
            _0: t98,
        }
        retv95 = t99
        return retv95
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
