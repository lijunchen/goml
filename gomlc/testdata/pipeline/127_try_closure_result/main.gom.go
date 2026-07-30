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
    var retv117 Result__string__string
    var jp119 Result__string__string
    if ok__0 {
        var t120 Result__string__string = Ok{
            _0: "body",
        }
        jp119 = t120
    } else {
        var t121 Result__string__string = Err{
            _0: "parse failed",
        }
        jp119 = t121
    }
    retv117 = jp119
    return retv117
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv123 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t124 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv123 = t124
    return retv123
}

func show(res__5 Result__string__string) string {
    var retv126 string
    var jp128 string
    switch res__5.(type) {
    case Ok:
        var x111 string = res__5.(Ok)._0
        var value__6 string = x111
        var t129 string = "ok " + value__6
        jp128 = t129
    case Err:
        var x112 string = res__5.(Err)._0
        var err__7 string = x112
        var t130 string = "err " + err__7
        jp128 = t130
    default:
        panic("non-exhaustive match")
    }
    retv126 = jp128
    return retv126
}

func main0() struct{} {
    var t132 Result__string__string = decorate("outer", true)
    var t133 string = show(t132)
    println__T_string(t133)
    var t134 Result__string__string = decorate("outer", false)
    var t135 string = show(t134)
    println__T_string(t135)
    return struct{}{}
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

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env115 closure_env_run_0) Result__string__string {
    var retv142 Result__string__string
    var ok__2 bool = env115.ok_0
    var prefix__1 string = env115.prefix_1
    var mtmp108 Result__string__string = parse_text(ok__2)
    var jp144 string
    switch mtmp108.(type) {
    case Ok:
        var x109 string = mtmp108.(Ok)._0
        var try_value__12 string = x109
        jp144 = try_value__12
        var text__3 string = jp144
        var t145 string = prefix__1 + ":"
        var t146 string = t145 + text__3
        var t147 Result__string__string = Ok{
            _0: t146,
        }
        retv142 = t147
        return retv142
    case Err:
        var x110 string = mtmp108.(Err)._0
        var try_residual__12 string = x110
        var t148 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv142 = t148
        return retv142
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
