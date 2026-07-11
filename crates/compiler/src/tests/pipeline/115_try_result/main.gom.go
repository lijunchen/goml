package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
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

func parse_flag(flag__0 bool) Result__int32__string {
    var retv16 Result__int32__string
    var jp18 Result__int32__string
    if flag__0 {
        var t19 Result__int32__string = Ok{
            _0: 7,
        }
        jp18 = t19
    } else {
        var t20 Result__int32__string = Err{
            _0: "nope",
        }
        jp18 = t20
    }
    retv16 = jp18
    return retv16
}

func add(a__1 int32, b__2 int32) int32 {
    var retv22 int32
    var t23 int32 = a__1 + b__2
    retv22 = t23
    return retv22
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv25 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t26 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv25 = t26
    return retv25
}

func show(res__6 Result__int32__string) string {
    var retv28 string
    var jp30 string
    switch res__6.(type) {
    case Ok:
        var x10 int32 = res__6.(Ok)._0
        var value__7 int32 = x10
        var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t32 string = "ok=" + t31
        jp30 = t32
    case Err:
        var x11 string = res__6.(Err)._0
        var err__8 string = x11
        var t33 string = "err=" + err__8
        jp30 = t33
    default:
        panic("non-exhaustive match")
    }
    retv28 = jp30
    return retv28
}

func main0() struct{} {
    var t35 Result__int32__string = plus_one(true)
    var t36 string = show(t35)
    println__T_string(t36)
    var t37 Result__int32__string = plus_one(false)
    var t38 string = show(t37)
    println__T_string(t38)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__2)
    retv40 = t41
    return retv40
}

func println__T_string(value__1 string) struct{} {
    var t43 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t43)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv46 string
    retv46 = self__9
    return retv46
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env14 closure_env_run_0) Result__int32__string {
    var retv48 Result__int32__string
    var flag__3 bool = env14.flag_0
    var mtmp7 Result__int32__string = parse_flag(flag__3)
    var jp50 int32
    switch mtmp7.(type) {
    case Ok:
        var x8 int32 = mtmp7.(Ok)._0
        var try_value__15 int32 = x8
        jp50 = try_value__15
        var value__4 int32 = jp50
        var t51 int32 = add(value__4, 1)
        var t52 Result__int32__string = Ok{
            _0: t51,
        }
        retv48 = t52
        return retv48
    case Err:
        var x9 string = mtmp7.(Err)._0
        var try_residual__15 string = x9
        var t53 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv48 = t53
        return retv48
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
