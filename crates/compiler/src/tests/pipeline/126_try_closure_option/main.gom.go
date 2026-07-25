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
    var retv71 Option__int32
    var jp73 Option__int32
    if flag__0 {
        var t74 Option__int32 = Some{
            _0: 4,
        }
        jp73 = t74
    } else {
        jp73 = None{}
    }
    retv71 = jp73
    return retv71
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv76 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t77 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv76 = t77
    return retv76
}

func show(opt__5 Option__int32) string {
    var retv79 string
    var jp81 string
    switch opt__5.(type) {
    case None:
        jp81 = "none"
    case Some:
        var x66 int32 = opt__5.(Some)._0
        var value__6 int32 = x66
        var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t83 string = "some=" + t82
        jp81 = t83
    default:
        panic("non-exhaustive match")
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var t85 Option__int32 = with_base(3, true)
    var t86 string = show(t85)
    println__T_string(t86)
    var t87 Option__int32 = with_base(3, false)
    var t88 string = show(t87)
    println__T_string(t88)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv90 string
    var t91 string = _goml_runtime_core_int32_to_string(self__6)
    retv90 = t91
    return retv90
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv96 string
    retv96 = self__38
    return retv96
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env69 closure_env_run_0) Option__int32 {
    var retv98 Option__int32
    var flag__2 bool = env69.flag_0
    var base__1 int32 = env69.base_1
    var mtmp64 Option__int32 = maybe_value(flag__2)
    var jp100 int32
    switch mtmp64.(type) {
    case None:
        retv98 = None{}
        return retv98
    case Some:
        var x65 int32 = mtmp64.(Some)._0
        var try_value__11 int32 = x65
        jp100 = try_value__11
        var value__3 int32 = jp100
        var t101 int32 = value__3 + base__1
        var t102 Option__int32 = Some{
            _0: t101,
        }
        retv98 = t102
        return retv98
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
