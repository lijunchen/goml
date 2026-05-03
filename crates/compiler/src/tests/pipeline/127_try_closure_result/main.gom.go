package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
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
    var retv9 Result__string__string
    var jp11 Result__string__string
    if ok__0 {
        var t12 Result__string__string = Ok{
            _0: "body",
        }
        jp11 = t12
    } else {
        var t13 Result__string__string = Err{
            _0: "parse failed",
        }
        jp11 = t13
    }
    retv9 = jp11
    return retv9
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv15 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t16 Result__string__string = _goml_inherent_x23_closure_x5f_env_x5f_run_x5f_0_x23_closure_x5f_env_x5f_run_x5f_0_x23_apply(run__4)
    retv15 = t16
    return retv15
}

func show(res__5 Result__string__string) string {
    var retv18 string
    var jp20 string
    switch res__5.(type) {
    case Ok:
        var x3 string = res__5.(Ok)._0
        var value__6 string = x3
        var t21 string = "ok " + value__6
        jp20 = t21
    case Err:
        var x4 string = res__5.(Err)._0
        var err__7 string = x4
        var t22 string = "err " + err__7
        jp20 = t22
    default:
        panic("non-exhaustive match")
    }
    retv18 = jp20
    return retv18
}

func main0() struct{} {
    var t24 Result__string__string = decorate("outer", true)
    var t25 string = show(t24)
    println__T_string(t25)
    var t26 Result__string__string = decorate("outer", false)
    var t27 string = show(t26)
    println__T_string(t27)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_run_x5f_0_x23_closure_x5f_env_x5f_run_x5f_0_x23_apply(env7 closure_env_run_0) Result__string__string {
    var retv31 Result__string__string
    var ok__2 bool = env7.ok_0
    var prefix__1 string = env7.prefix_1
    var mtmp0 Result__string__string = parse_text(ok__2)
    var jp33 string
    switch mtmp0.(type) {
    case Ok:
        var x1 string = mtmp0.(Ok)._0
        var try_value__12 string = x1
        jp33 = try_value__12
        var text__3 string = jp33
        var t34 string = prefix__1 + ":"
        var t35 string = t34 + text__3
        var t36 Result__string__string = Ok{
            _0: t35,
        }
        retv31 = t36
        return retv31
    case Err:
        var x2 string = mtmp0.(Err)._0
        var try_residual__12 string = x2
        var t37 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv31 = t37
        return retv31
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
