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
    var retv73 Result__string__string
    var jp75 Result__string__string
    if ok__0 {
        var t76 Result__string__string = Ok{
            _0: "body",
        }
        jp75 = t76
    } else {
        var t77 Result__string__string = Err{
            _0: "parse failed",
        }
        jp75 = t77
    }
    retv73 = jp75
    return retv73
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv79 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t80 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv79 = t80
    return retv79
}

func show(res__5 Result__string__string) string {
    var retv82 string
    var jp84 string
    switch res__5.(type) {
    case Ok:
        var x67 string = res__5.(Ok)._0
        var value__6 string = x67
        var t85 string = "ok " + value__6
        jp84 = t85
    case Err:
        var x68 string = res__5.(Err)._0
        var err__7 string = x68
        var t86 string = "err " + err__7
        jp84 = t86
    default:
        panic("non-exhaustive match")
    }
    retv82 = jp84
    return retv82
}

func main0() struct{} {
    var t88 Result__string__string = decorate("outer", true)
    var t89 string = show(t88)
    println__T_string(t89)
    var t90 Result__string__string = decorate("outer", false)
    var t91 string = show(t90)
    println__T_string(t91)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv96 string
    retv96 = self__38
    return retv96
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env71 closure_env_run_0) Result__string__string {
    var retv98 Result__string__string
    var ok__2 bool = env71.ok_0
    var prefix__1 string = env71.prefix_1
    var mtmp64 Result__string__string = parse_text(ok__2)
    var jp100 string
    switch mtmp64.(type) {
    case Ok:
        var x65 string = mtmp64.(Ok)._0
        var try_value__12 string = x65
        jp100 = try_value__12
        var text__3 string = jp100
        var t101 string = prefix__1 + ":"
        var t102 string = t101 + text__3
        var t103 Result__string__string = Ok{
            _0: t102,
        }
        retv98 = t103
        return retv98
    case Err:
        var x66 string = mtmp64.(Err)._0
        var try_residual__12 string = x66
        var t104 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv98 = t104
        return retv98
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
