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
    var retv164 Result__int32__string
    var jp166 Result__int32__string
    if flag__0 {
        var t167 Result__int32__string = Ok{
            _0: 7,
        }
        jp166 = t167
    } else {
        var t168 Result__int32__string = Err{
            _0: "nope",
        }
        jp166 = t168
    }
    retv164 = jp166
    return retv164
}

func add(a__1 int32, b__2 int32) int32 {
    var retv170 int32
    var t171 int32 = a__1 + b__2
    retv170 = t171
    return retv170
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv173 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t174 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv173 = t174
    return retv173
}

func show(res__6 Result__int32__string) string {
    var retv176 string
    var jp178 string
    switch res__6.(type) {
    case Ok:
        var x158 int32 = res__6.(Ok)._0
        var value__7 int32 = x158
        var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t180 string = "ok=" + t179
        jp178 = t180
    case Err:
        var x159 string = res__6.(Err)._0
        var err__8 string = x159
        var t181 string = "err=" + err__8
        jp178 = t181
    default:
        panic("non-exhaustive match")
    }
    retv176 = jp178
    return retv176
}

func main0() struct{} {
    var t183 Result__int32__string = plus_one(true)
    var t184 string = show(t183)
    println__T_string(t184)
    var t185 Result__int32__string = plus_one(false)
    var t186 string = show(t185)
    println__T_string(t186)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv188 string
    var t189 string = _goml_runtime_core_int32_to_string(self__6)
    retv188 = t189
    return retv188
}

func println__T_string(value__1 string) struct{} {
    var t191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t191)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv194 string
    retv194 = self__38
    return retv194
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env162 closure_env_run_0) Result__int32__string {
    var retv196 Result__int32__string
    var flag__3 bool = env162.flag_0
    var mtmp155 Result__int32__string = parse_flag(flag__3)
    var jp198 int32
    switch mtmp155.(type) {
    case Ok:
        var x156 int32 = mtmp155.(Ok)._0
        var try_value__15 int32 = x156
        jp198 = try_value__15
        var value__4 int32 = jp198
        var t199 int32 = add(value__4, 1)
        var t200 Result__int32__string = Ok{
            _0: t199,
        }
        retv196 = t200
        return retv196
    case Err:
        var x157 string = mtmp155.(Err)._0
        var try_residual__15 string = x157
        var t201 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv196 = t201
        return retv196
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
