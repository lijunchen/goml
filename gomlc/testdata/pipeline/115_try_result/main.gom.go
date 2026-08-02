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
    if flag__0 {
        var t167 Result__int32__string = Ok{
            _0: 7,
        }
        return t167
    } else {
        var t168 Result__int32__string = Err{
            _0: "nope",
        }
        return t168
    }
}

func add(a__1 int32, b__2 int32) int32 {
    var t171 int32 = a__1 + b__2
    return t171
}

func plus_one(flag__3 bool) Result__int32__string {
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t174 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    return t174
}

func show(res__6 Result__int32__string) string {
    switch res__6.(type) {
    case Ok:
        var x158 int32 = res__6.(Ok)._0
        var t179 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x158)
        var t180 string = "ok=" + t179
        return t180
    case Err:
        var x159 string = res__6.(Err)._0
        var t181 string = "err=" + x159
        return t181
    default:
        panic("non-exhaustive match")
    }
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
    var t189 string = _goml_runtime_core_int32_to_string(self__6)
    return t189
}

func println__T_string(value__1 string) struct{} {
    var t191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t191)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env162 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env162.flag_0
    var mtmp155 Result__int32__string = parse_flag(flag__3)
    var jp198 int32
    switch mtmp155.(type) {
    case Ok:
        var x156 int32 = mtmp155.(Ok)._0
        jp198 = x156
        var t199 int32 = add(jp198, 1)
        var t200 Result__int32__string = Ok{
            _0: t199,
        }
        return t200
    case Err:
        var x157 string = mtmp155.(Err)._0
        var t201 Result__int32__string = Err{
            _0: x157,
        }
        return t201
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
