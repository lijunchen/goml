package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

func maybe_value(flag__0 bool) Option__int32 {
    var retv7 Option__int32
    var jp9 Option__int32
    if flag__0 {
        var t10 Option__int32 = Some{
            _0: 4,
        }
        jp9 = t10
    } else {
        jp9 = None{}
    }
    retv7 = jp9
    return retv7
}

func with_base(base__1 int32, flag__2 bool) Option__int32 {
    var retv12 Option__int32
    var run__4 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__2,
        base_1: base__1,
    }
    var t13 Option__int32 = _goml_inherent_x23_closure_x5f_env_x5f_run_x5f_0_x23_closure_x5f_env_x5f_run_x5f_0_x23_apply(run__4)
    retv12 = t13
    return retv12
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

func _goml_inherent_x23_closure_x5f_env_x5f_run_x5f_0_x23_closure_x5f_env_x5f_run_x5f_0_x23_apply(env5 closure_env_run_0) Option__int32 {
    var retv28 Option__int32
    var flag__2 bool = env5.flag_0
    var base__1 int32 = env5.base_1
    var mtmp0 Option__int32 = maybe_value(flag__2)
    var jp30 int32
    switch mtmp0.(type) {
    case None:
        retv28 = None{}
        return retv28
    case Some:
        var x1 int32 = mtmp0.(Some)._0
        var try_value__11 int32 = x1
        jp30 = try_value__11
        var value__3 int32 = jp30
        var t31 int32 = value__3 + base__1
        var t32 Option__int32 = Some{
            _0: t31,
        }
        retv28 = t32
        return retv28
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
