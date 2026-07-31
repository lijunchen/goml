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
    var retv159 Option__int32
    var jp161 Option__int32
    if flag__0 {
        var t162 Option__int32 = Some{
            _0: 4,
        }
        jp161 = t162
    } else {
        jp161 = None{}
    }
    retv159 = jp161
    return retv159
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv164 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t165 Option__int32 = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv164 = t165
    return retv164
}

func show(opt__5 Option__int32) string {
    var retv167 string
    var jp169 string
    switch opt__5.(type) {
    case None:
        jp169 = "none"
    case Some:
        var x154 int32 = opt__5.(Some)._0
        var value__6 int32 = x154
        var t170 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__6)
        var t171 string = "some=" + t170
        jp169 = t171
    default:
        panic("non-exhaustive match")
    }
    retv167 = jp169
    return retv167
}

func main0() struct{} {
    var t173 Option__int32 = with_base(3, true)
    var t174 string = show(t173)
    println__T_string(t174)
    var t175 Option__int32 = with_base(3, false)
    var t176 string = show(t175)
    println__T_string(t176)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv178 string
    var t179 string = _goml_runtime_core_int32_to_string(self__6)
    retv178 = t179
    return retv178
}

func println__T_string(value__1 string) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv184 string
    retv184 = self__38
    return retv184
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env157 closure_env_run_0) Option__int32 {
    var retv186 Option__int32
    var flag__2 bool = env157.flag_0
    var base__1 int32 = env157.base_1
    var mtmp152 Option__int32 = maybe_value(flag__2)
    var jp188 int32
    switch mtmp152.(type) {
    case None:
        retv186 = None{}
        return retv186
    case Some:
        var x153 int32 = mtmp152.(Some)._0
        var try_value__11 int32 = x153
        jp188 = try_value__11
        var value__3 int32 = jp188
        var t189 int32 = value__3 + base__1
        var t190 Option__int32 = Some{
            _0: t189,
        }
        retv186 = t190
        return retv186
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
