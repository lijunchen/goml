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
    var retv115 Option__int32
    var jp117 Option__int32
    if flag__0 {
        var t118 Option__int32 = Some{
            _0: 4,
        }
        jp117 = t118
    } else {
        jp117 = None{}
    }
    retv115 = jp117
    return retv115
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv120 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t121 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv120 = t121
    return retv120
}

func show(opt__5 Option__int32) string {
    var retv123 string
    var jp125 string
    switch opt__5.(type) {
    case None:
        jp125 = "none"
    case Some:
        var x110 int32 = opt__5.(Some)._0
        var value__6 int32 = x110
        var t126 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t127 string = "some=" + t126
        jp125 = t127
    default:
        panic("non-exhaustive match")
    }
    retv123 = jp125
    return retv123
}

func main0() struct{} {
    var t129 Option__int32 = with_base(3, true)
    var t130 string = show(t129)
    println__T_string(t130)
    var t131 Option__int32 = with_base(3, false)
    var t132 string = show(t131)
    println__T_string(t132)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv134 string
    var t135 string = _goml_runtime_core_int32_to_string(self__6)
    retv134 = t135
    return retv134
}

func println__T_string(value__1 string) struct{} {
    var t137 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t137)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv140 string
    retv140 = self__38
    return retv140
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env113 closure_env_run_0) Option__int32 {
    var retv142 Option__int32
    var flag__2 bool = env113.flag_0
    var base__1 int32 = env113.base_1
    var mtmp108 Option__int32 = maybe_value(flag__2)
    var jp144 int32
    switch mtmp108.(type) {
    case None:
        retv142 = None{}
        return retv142
    case Some:
        var x109 int32 = mtmp108.(Some)._0
        var try_value__11 int32 = x109
        jp144 = try_value__11
        var value__3 int32 = jp144
        var t145 int32 = value__3 + base__1
        var t146 Option__int32 = Some{
            _0: t145,
        }
        retv142 = t146
        return retv142
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
