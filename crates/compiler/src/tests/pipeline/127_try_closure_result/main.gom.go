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
    var retv31 Result__string__string
    var jp33 Result__string__string
    if ok__0 {
        var t34 Result__string__string = Ok{
            _0: "body",
        }
        jp33 = t34
    } else {
        var t35 Result__string__string = Err{
            _0: "parse failed",
        }
        jp33 = t35
    }
    retv31 = jp33
    return retv31
}

func decorate(prefix__1 string, ok__2 bool) Result__string__string {
    var retv37 Result__string__string
    var run__4 closure_env_run_0 = closure_env_run_0{
        ok_0: ok__2,
        prefix_1: prefix__1,
    }
    var t38 Result__string__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__4)
    retv37 = t38
    return retv37
}

func show(res__5 Result__string__string) string {
    var retv40 string
    var jp42 string
    switch res__5.(type) {
    case Ok:
        var x25 string = res__5.(Ok)._0
        var value__6 string = x25
        var t43 string = "ok " + value__6
        jp42 = t43
    case Err:
        var x26 string = res__5.(Err)._0
        var err__7 string = x26
        var t44 string = "err " + err__7
        jp42 = t44
    default:
        panic("non-exhaustive match")
    }
    retv40 = jp42
    return retv40
}

func main0() struct{} {
    var t46 Result__string__string = decorate("outer", true)
    var t47 string = show(t46)
    println__T_string(t47)
    var t48 Result__string__string = decorate("outer", false)
    var t49 string = show(t48)
    println__T_string(t49)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t51 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t51)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env29 closure_env_run_0) Result__string__string {
    var retv56 Result__string__string
    var ok__2 bool = env29.ok_0
    var prefix__1 string = env29.prefix_1
    var mtmp22 Result__string__string = parse_text(ok__2)
    var jp58 string
    switch mtmp22.(type) {
    case Ok:
        var x23 string = mtmp22.(Ok)._0
        var try_value__12 string = x23
        jp58 = try_value__12
        var text__3 string = jp58
        var t59 string = prefix__1 + ":"
        var t60 string = t59 + text__3
        var t61 Result__string__string = Ok{
            _0: t60,
        }
        retv56 = t61
        return retv56
    case Err:
        var x24 string = mtmp22.(Err)._0
        var try_residual__12 string = x24
        var t62 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv56 = t62
        return retv56
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
