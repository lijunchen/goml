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
    var retv77 Result__string__string
    var jp79 Result__string__string
    if ok__0 {
        var t80 Result__string__string = Ok{
            _0: "body",
        }
        jp79 = t80
    } else {
        var t81 Result__string__string = Err{
            _0: "parse failed",
        }
        jp79 = t81
    }
    retv77 = jp79
    return retv77
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv83 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t84 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv83 = t84
    return retv83
}

func show(res__5 Result__string__string) string {
    var retv86 string
    var jp88 string
    switch res__5.(type) {
    case Ok:
        var x71 string = res__5.(Ok)._0
        var value__6 string = x71
        var t89 string = "ok " + value__6
        jp88 = t89
    case Err:
        var x72 string = res__5.(Err)._0
        var err__7 string = x72
        var t90 string = "err " + err__7
        jp88 = t90
    default:
        panic("non-exhaustive match")
    }
    retv86 = jp88
    return retv86
}

func main0() struct{} {
    var t92 Result__string__string = decorate("outer", true)
    var t93 string = show(t92)
    println__T_string(t93)
    var t94 Result__string__string = decorate("outer", false)
    var t95 string = show(t94)
    println__T_string(t95)
    return struct{}{}
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

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env75 closure_env_run_0) Result__string__string {
    var retv102 Result__string__string
    var ok__2 bool = env75.ok_0
    var prefix__1 string = env75.prefix_1
    var mtmp68 Result__string__string = parse_text(ok__2)
    var jp104 string
    switch mtmp68.(type) {
    case Ok:
        var x69 string = mtmp68.(Ok)._0
        var try_value__12 string = x69
        jp104 = try_value__12
        var text__3 string = jp104
        var t105 string = prefix__1 + ":"
        var t106 string = t105 + text__3
        var t107 Result__string__string = Ok{
            _0: t106,
        }
        retv102 = t107
        return retv102
    case Err:
        var x70 string = mtmp68.(Err)._0
        var try_residual__12 string = x70
        var t108 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv102 = t108
        return retv102
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
