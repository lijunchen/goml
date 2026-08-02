package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_run_0 struct {
    ok_0 bool
    prefix_1 string
}

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func parse_text(ok__0 bool) Result__string__string {
    if ok__0 {
        var t167 Result__string__string = Ok{
            _0: "body",
        }
        return t167
    } else {
        var t168 Result__string__string = Err{
            _0: "parse failed",
        }
        return t168
    }
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t171 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    return t171
}

func show(res__5 Result__string__string) string {
    switch res__5.(type) {
    case Ok:
        var x158 string = res__5.(Ok)._0
        var t176 string = "ok " + x158
        return t176
    case Err:
        var x159 string = res__5.(Err)._0
        var t177 string = "err " + x159
        return t177
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t179 Result__string__string = decorate("outer", true)
    var t180 string = show(t179)
    println__T_string(t180)
    var t181 Result__string__string = decorate("outer", false)
    var t182 string = show(t181)
    println__T_string(t182)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env162 closure_env_run_0) Result__string__string {
    var ok__2 bool = env162.ok_0
    var prefix__1 string = env162.prefix_1
    var mtmp155 Result__string__string = parse_text(ok__2)
    var jp191 string
    switch mtmp155.(type) {
    case Ok:
        var x156 string = mtmp155.(Ok)._0
        jp191 = x156
        var t192 string = prefix__1 + ":"
        var t193 string = t192 + jp191
        var t194 Result__string__string = Ok{
            _0: t193,
        }
        return t194
    case Err:
        var x157 string = mtmp155.(Err)._0
        var t195 Result__string__string = Err{
            _0: x157,
        }
        return t195
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
