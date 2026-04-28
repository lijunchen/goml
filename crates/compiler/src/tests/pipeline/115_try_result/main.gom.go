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
}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

type GoError = error

func parse_flag(flag__0 bool) Result__int32__string {
    var retv9 Result__int32__string
    var jp11 Result__int32__string
    if flag__0 {
        var t12 Result__int32__string = Ok{
            _0: 7,
        }
        jp11 = t12
    } else {
        var t13 Result__int32__string = Err{
            _0: "nope",
        }
        jp11 = t13
    }
    retv9 = jp11
    return retv9
}

func add(a__1 int32, b__2 int32) int32 {
    var retv15 int32
    var t16 int32 = a__1 + b__2
    retv15 = t16
    return retv15
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv18 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t19 Result__int32__string = _goml_inherent_x23_closure_x5f_env_x5f_run_x5f_0_x23_closure_x5f_env_x5f_run_x5f_0_x23_apply(run__5)
    retv18 = t19
    return retv18
}

func show(res__6 Result__int32__string) string {
    var retv21 string
    var jp23 string
    switch res__6.(type) {
    case Ok:
        var x3 int32 = res__6.(Ok)._0
        var value__7 int32 = x3
        var t24 string = int32_to_string(value__7)
        var t25 string = "ok=" + t24
        jp23 = t25
    case Err:
        var x4 string = res__6.(Err)._0
        var err__8 string = x4
        var t26 string = "err=" + err__8
        jp23 = t26
    default:
        panic("non-exhaustive match")
    }
    retv21 = jp23
    return retv21
}

func main0() struct{} {
    var t28 Result__int32__string = plus_one(true)
    var t29 string = show(t28)
    println__T_string(t29)
    var t30 Result__int32__string = plus_one(false)
    var t31 string = show(t30)
    println__T_string(t31)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_run_x5f_0_x23_closure_x5f_env_x5f_run_x5f_0_x23_apply(env7 closure_env_run_0) Result__int32__string {
    var retv35 Result__int32__string
    var flag__3 bool = env7.flag_0
    var mtmp0 Result__int32__string = parse_flag(flag__3)
    var jp37 int32
    switch mtmp0.(type) {
    case Ok:
        var x1 int32 = mtmp0.(Ok)._0
        var try_value__15 int32 = x1
        jp37 = try_value__15
        var value__4 int32 = jp37
        var t38 int32 = add(value__4, 1)
        var t39 Result__int32__string = Ok{
            _0: t38,
        }
        retv35 = t39
        return retv35
    case Err:
        var x2 string = mtmp0.(Err)._0
        var try_residual__15 string = x2
        var t40 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv35 = t40
        return retv35
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
