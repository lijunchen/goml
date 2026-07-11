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
    var retv13 Result__string__string
    var jp15 Result__string__string
    if ok__0 {
        var t16 Result__string__string = Ok{
            _0: "body",
        }
        jp15 = t16
    } else {
        var t17 Result__string__string = Err{
            _0: "parse failed",
        }
        jp15 = t17
    }
    retv13 = jp15
    return retv13
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv19 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t20 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv19 = t20
    return retv19
}

func show(res__5 Result__string__string) string {
    var retv22 string
    var jp24 string
    switch res__5.(type) {
    case Ok:
        var x7 string = res__5.(Ok)._0
        var value__6 string = x7
        var t25 string = "ok " + value__6
        jp24 = t25
    case Err:
        var x8 string = res__5.(Err)._0
        var err__7 string = x8
        var t26 string = "err " + err__7
        jp24 = t26
    default:
        panic("non-exhaustive match")
    }
    retv22 = jp24
    return retv22
}

func main0() struct{} {
    var t28 Result__string__string = decorate("outer", true)
    var t29 string = show(t28)
    println__T_string(t29)
    var t30 Result__string__string = decorate("outer", false)
    var t31 string = show(t30)
    println__T_string(t31)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv36 string
    retv36 = self__9
    return retv36
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env11 closure_env_run_0) Result__string__string {
    var retv38 Result__string__string
    var ok__2 bool = env11.ok_0
    var prefix__1 string = env11.prefix_1
    var mtmp4 Result__string__string = parse_text(ok__2)
    var jp40 string
    switch mtmp4.(type) {
    case Ok:
        var x5 string = mtmp4.(Ok)._0
        var try_value__12 string = x5
        jp40 = try_value__12
        var text__3 string = jp40
        var t41 string = prefix__1 + ":"
        var t42 string = t41 + text__3
        var t43 Result__string__string = Ok{
            _0: t42,
        }
        retv38 = t43
        return retv38
    case Err:
        var x6 string = mtmp4.(Err)._0
        var try_residual__12 string = x6
        var t44 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv38 = t44
        return retv38
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
