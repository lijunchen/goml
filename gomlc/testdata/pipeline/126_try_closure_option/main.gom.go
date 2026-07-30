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
    var retv75 Option__int32
    var jp77 Option__int32
    if flag__0 {
        var t78 Option__int32 = Some{
            _0: 4,
        }
        jp77 = t78
    } else {
        jp77 = None{}
    }
    retv75 = jp77
    return retv75
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv80 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t81 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv80 = t81
    return retv80
}

func show(opt__5 Option__int32) string {
    var retv83 string
    var jp85 string
    switch opt__5.(type) {
    case None:
        jp85 = "none"
    case Some:
        var x70 int32 = opt__5.(Some)._0
        var value__6 int32 = x70
        var t86 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t87 string = "some=" + t86
        jp85 = t87
    default:
        panic("non-exhaustive match")
    }
    retv83 = jp85
    return retv83
}

func main0() struct{} {
    var t89 Option__int32 = with_base(3, true)
    var t90 string = show(t89)
    println__T_string(t90)
    var t91 Option__int32 = with_base(3, false)
    var t92 string = show(t91)
    println__T_string(t92)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__6)
    retv94 = t95
    return retv94
}

func println__T_string(value__1 string) struct{} {
    var t97 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t97)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv100 string
    retv100 = self__38
    return retv100
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env73 closure_env_run_0) Option__int32 {
    var retv102 Option__int32
    var flag__2 bool = env73.flag_0
    var base__1 int32 = env73.base_1
    var mtmp68 Option__int32 = maybe_value(flag__2)
    var jp104 int32
    switch mtmp68.(type) {
    case None:
        retv102 = None{}
        return retv102
    case Some:
        var x69 int32 = mtmp68.(Some)._0
        var try_value__11 int32 = x69
        jp104 = try_value__11
        var value__3 int32 = jp104
        var t105 int32 = value__3 + base__1
        var t106 Option__int32 = Some{
            _0: t105,
        }
        retv102 = t106
        return retv102
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
