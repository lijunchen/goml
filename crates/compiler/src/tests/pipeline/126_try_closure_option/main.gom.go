package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type closure_env_run_0 struct {
    flag_0 bool
    base_1 int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type GoError = error

func maybe_value__native(flag__0 bool) (int32, bool) {
    if flag__0 {
        return 4, true
    } else {
        var ret_zero int32
        return ret_zero, false
    }
}

func with_base__native(base__1 int32, flag__2 bool) (int32, bool) {
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    _goml_inherent_closure_env_run_0_closure_env_run_0_apply(run__4)
    var t13_value int32
    var t13_ok bool
    t13_value, t13_ok = _goml_inherent_closure_env_run_0_closure_env_run_0_apply__native(run__4)
    if !t13_ok {
        var ret_zero int32
        return ret_zero, false
    }
    return t13_value, true
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var native_value int32
    var native_ok bool
    native_value, native_ok = with_base__native(base__1, flag__2)
    if native_ok {
        return Some{
            _0: native_value,
        }
    }
    return None{}
}

func show(opt__5 Option__int32) string {
    var retv15 string
    var jp17 string
    switch opt__5.(type) {
    case None:
        jp17 = "none"
    case Some:
        var x2 int32 = opt__5.(Some)._0
        var value__6 int32 = x2
        var t18 string = int32_to_string(value__6)
        var t19 string = "some=" + t18
        jp17 = t19
    default:
        panic("non-exhaustive match")
    }
    retv15 = jp17
    return retv15
}

func main0() struct{} {
    var t21 Option__int32 = with_base(3, true)
    var t22 string = show(t21)
    println__T_string(t22)
    var t23 Option__int32 = with_base(3, false)
    var t24 string = show(t23)
    println__T_string(t24)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_closure_env_run_0_closure_env_run_0_apply__native(env5 closure_env_run_0) (int32, bool) {
    var flag__2 bool = env5.flag_0
    var base__1 int32 = env5.base_1
    var jp30 int32
    var mtmp0_value int32
    var mtmp0_ok bool
    mtmp0_value, mtmp0_ok = maybe_value__native(flag__2)
    if !mtmp0_ok {
        var ret_zero int32
        return ret_zero, false
    }
    jp30 = mtmp0_value
    var value__3 int32 = jp30
    var t31 int32 = value__3 + base__1
    return t31, true
}

func _goml_inherent_closure_env_run_0_closure_env_run_0_apply(env5 closure_env_run_0) Option__int32 {
    var native_value int32
    var native_ok bool
    native_value, native_ok = _goml_inherent_closure_env_run_0_closure_env_run_0_apply__native(env5)
    if native_ok {
        return Some{
            _0: native_value,
        }
    }
    return None{}
}

func main() {
    main0()
}
