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
    var retv67 Result__string__string
    var jp69 Result__string__string
    if ok__0 {
        var t70 Result__string__string = Ok{
            _0: "body",
        }
        jp69 = t70
    } else {
        var t71 Result__string__string = Err{
            _0: "parse failed",
        }
        jp69 = t71
    }
    retv67 = jp69
    return retv67
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv73 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t74 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv73 = t74
    return retv73
}

func show(res__5 Result__string__string) string {
    var retv76 string
    var jp78 string
    switch res__5.(type) {
    case Ok:
        var x61 string = res__5.(Ok)._0
        var value__6 string = x61
        var t79 string = "ok " + value__6
        jp78 = t79
    case Err:
        var x62 string = res__5.(Err)._0
        var err__7 string = x62
        var t80 string = "err " + err__7
        jp78 = t80
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func main0() struct{} {
    var t82 Result__string__string = decorate("outer", true)
    var t83 string = show(t82)
    println__T_string(t83)
    var t84 Result__string__string = decorate("outer", false)
    var t85 string = show(t84)
    println__T_string(t85)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv90 string
    retv90 = self__34
    return retv90
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env65 closure_env_run_0) Result__string__string {
    var retv92 Result__string__string
    var ok__2 bool = env65.ok_0
    var prefix__1 string = env65.prefix_1
    var mtmp58 Result__string__string = parse_text(ok__2)
    var jp94 string
    switch mtmp58.(type) {
    case Ok:
        var x59 string = mtmp58.(Ok)._0
        var try_value__12 string = x59
        jp94 = try_value__12
        var text__3 string = jp94
        var t95 string = prefix__1 + ":"
        var t96 string = t95 + text__3
        var t97 Result__string__string = Ok{
            _0: t96,
        }
        retv92 = t97
        return retv92
    case Err:
        var x60 string = mtmp58.(Err)._0
        var try_residual__12 string = x60
        var t98 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv92 = t98
        return retv92
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
