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
    var retv13 Result__int32__string
    var jp15 Result__int32__string
    if flag__0 {
        var t16 Result__int32__string = Ok{
            _0: 7,
        }
        jp15 = t16
    } else {
        var t17 Result__int32__string = Err{
            _0: "nope",
        }
        jp15 = t17
    }
    retv13 = jp15
    return retv13
}

func add(a__1 int32, b__2 int32) int32 {
    var retv19 int32
    var t20 int32 = a__1 + b__2
    retv19 = t20
    return retv19
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv22 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t23 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv22 = t23
    return retv22
}

func show(res__6 Result__int32__string) string {
    var retv25 string
    var jp27 string
    switch res__6.(type) {
    case Ok:
        var x7 int32 = res__6.(Ok)._0
        var value__7 int32 = x7
        var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t29 string = "ok=" + t28
        jp27 = t29
    case Err:
        var x8 string = res__6.(Err)._0
        var err__8 string = x8
        var t30 string = "err=" + err__8
        jp27 = t30
    default:
        panic("non-exhaustive match")
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var t32 Result__int32__string = plus_one(true)
    var t33 string = show(t32)
    println__T_string(t33)
    var t34 Result__int32__string = plus_one(false)
    var t35 string = show(t34)
    println__T_string(t35)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv37 string
    var t38 string = _goml_runtime_core_int32_to_string(self__2)
    retv37 = t38
    return retv37
}

func println__T_string(value__1 string) struct{} {
    var t40 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t40)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv43 string
    retv43 = self__9
    return retv43
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env11 closure_env_run_0) Result__int32__string {
    var retv45 Result__int32__string
    var flag__3 bool = env11.flag_0
    var mtmp4 Result__int32__string = parse_flag(flag__3)
    var jp47 int32
    switch mtmp4.(type) {
    case Ok:
        var x5 int32 = mtmp4.(Ok)._0
        var try_value__15 int32 = x5
        jp47 = try_value__15
        var value__4 int32 = jp47
        var t48 int32 = add(value__4, 1)
        var t49 Result__int32__string = Ok{
            _0: t48,
        }
        retv45 = t49
        return retv45
    case Err:
        var x6 string = mtmp4.(Err)._0
        var try_residual__15 string = x6
        var t50 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv45 = t50
        return retv45
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
