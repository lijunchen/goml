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
}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func parse_flag(flag__0 bool) Result__int32__string {
    var retv161 Result__int32__string
    var jp163 Result__int32__string
    if flag__0 {
        var t164 Result__int32__string = Ok{
            _0: 7,
        }
        jp163 = t164
    } else {
        var t165 Result__int32__string = Err{
            _0: "nope",
        }
        jp163 = t165
    }
    retv161 = jp163
    return retv161
}

func add(a__1 int32, b__2 int32) int32 {
    var retv167 int32
    var t168 int32 = a__1 + b__2
    retv167 = t168
    return retv167
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv170 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t171 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv170 = t171
    return retv170
}

func show(res__6 Result__int32__string) string {
    var retv173 string
    var jp175 string
    switch res__6.(type) {
    case Ok:
        var x155 int32 = res__6.(Ok)._0
        var value__7 int32 = x155
        var t176 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t177 string = "ok=" + t176
        jp175 = t177
    case Err:
        var x156 string = res__6.(Err)._0
        var err__8 string = x156
        var t178 string = "err=" + err__8
        jp175 = t178
    default:
        panic("non-exhaustive match")
    }
    retv173 = jp175
    return retv173
}

func main0() struct{} {
    var t180 Result__int32__string = plus_one(true)
    var t181 string = show(t180)
    println__T_string(t181)
    var t182 Result__int32__string = plus_one(false)
    var t183 string = show(t182)
    println__T_string(t183)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv185 string
    var t186 string = _goml_runtime_core_int32_to_string(self__6)
    retv185 = t186
    return retv185
}

func println__T_string(value__1 string) struct{} {
    var t188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t188)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv191 string
    retv191 = self__38
    return retv191
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env159 closure_env_run_0) Result__int32__string {
    var retv193 Result__int32__string
    var flag__3 bool = env159.flag_0
    var mtmp152 Result__int32__string = parse_flag(flag__3)
    var jp195 int32
    switch mtmp152.(type) {
    case Ok:
        var x153 int32 = mtmp152.(Ok)._0
        var try_value__15 int32 = x153
        jp195 = try_value__15
        var value__4 int32 = jp195
        var t196 int32 = add(value__4, 1)
        var t197 Result__int32__string = Ok{
            _0: t196,
        }
        retv193 = t197
        return retv193
    case Err:
        var x154 string = mtmp152.(Err)._0
        var try_residual__15 string = x154
        var t198 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv193 = t198
        return retv193
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
