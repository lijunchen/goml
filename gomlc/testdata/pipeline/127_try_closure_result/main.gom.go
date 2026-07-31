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
    var retv161 Result__string__string
    var jp163 Result__string__string
    if ok__0 {
        var t164 Result__string__string = Ok{
            _0: "body",
        }
        jp163 = t164
    } else {
        var t165 Result__string__string = Err{
            _0: "parse failed",
        }
        jp163 = t165
    }
    retv161 = jp163
    return retv161
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv167 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t168 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv167 = t168
    return retv167
}

func show(res__5 Result__string__string) string {
    var retv170 string
    var jp172 string
    switch res__5.(type) {
    case Ok:
        var x155 string = res__5.(Ok)._0
        var value__6 string = x155
        var t173 string = "ok " + value__6
        jp172 = t173
    case Err:
        var x156 string = res__5.(Err)._0
        var err__7 string = x156
        var t174 string = "err " + err__7
        jp172 = t174
    default:
        panic("non-exhaustive match")
    }
    retv170 = jp172
    return retv170
}

func main0() struct{} {
    var t176 Result__string__string = decorate("outer", true)
    var t177 string = show(t176)
    println__T_string(t177)
    var t178 Result__string__string = decorate("outer", false)
    var t179 string = show(t178)
    println__T_string(t179)
    return struct{}{}
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

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env159 closure_env_run_0) Result__string__string {
    var retv186 Result__string__string
    var ok__2 bool = env159.ok_0
    var prefix__1 string = env159.prefix_1
    var mtmp152 Result__string__string = parse_text(ok__2)
    var jp188 string
    switch mtmp152.(type) {
    case Ok:
        var x153 string = mtmp152.(Ok)._0
        var try_value__12 string = x153
        jp188 = try_value__12
        var text__3 string = jp188
        var t189 string = prefix__1 + ":"
        var t190 string = t189 + text__3
        var t191 Result__string__string = Ok{
            _0: t190,
        }
        retv186 = t191
        return retv186
    case Err:
        var x154 string = mtmp152.(Err)._0
        var try_residual__12 string = x154
        var t192 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv186 = t192
        return retv186
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
