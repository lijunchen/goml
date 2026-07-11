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
    var retv16 Result__string__string
    var jp18 Result__string__string
    if ok__0 {
        var t19 Result__string__string = Ok{
            _0: "body",
        }
        jp18 = t19
    } else {
        var t20 Result__string__string = Err{
            _0: "parse failed",
        }
        jp18 = t20
    }
    retv16 = jp18
    return retv16
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv22 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t23 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv22 = t23
    return retv22
}

func show(res__5 Result__string__string) string {
    var retv25 string
    var jp27 string
    switch res__5.(type) {
    case Ok:
        var x10 string = res__5.(Ok)._0
        var value__6 string = x10
        var t28 string = "ok " + value__6
        jp27 = t28
    case Err:
        var x11 string = res__5.(Err)._0
        var err__7 string = x11
        var t29 string = "err " + err__7
        jp27 = t29
    default:
        panic("non-exhaustive match")
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var t31 Result__string__string = decorate("outer", true)
    var t32 string = show(t31)
    println__T_string(t32)
    var t33 Result__string__string = decorate("outer", false)
    var t34 string = show(t33)
    println__T_string(t34)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env14 closure_env_run_0) Result__string__string {
    var retv41 Result__string__string
    var ok__2 bool = env14.ok_0
    var prefix__1 string = env14.prefix_1
    var mtmp7 Result__string__string = parse_text(ok__2)
    var jp43 string
    switch mtmp7.(type) {
    case Ok:
        var x8 string = mtmp7.(Ok)._0
        var try_value__12 string = x8
        jp43 = try_value__12
        var text__3 string = jp43
        var t44 string = prefix__1 + ":"
        var t45 string = t44 + text__3
        var t46 Result__string__string = Ok{
            _0: t45,
        }
        retv41 = t46
        return retv41
    case Err:
        var x9 string = mtmp7.(Err)._0
        var try_residual__12 string = x9
        var t47 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv41 = t47
        return retv41
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
