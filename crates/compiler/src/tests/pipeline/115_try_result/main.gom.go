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
    var retv31 Result__int32__string
    var jp33 Result__int32__string
    if flag__0 {
        var t34 Result__int32__string = Ok{
            _0: 7,
        }
        jp33 = t34
    } else {
        var t35 Result__int32__string = Err{
            _0: "nope",
        }
        jp33 = t35
    }
    retv31 = jp33
    return retv31
}

func add(a__1 int32, b__2 int32) int32 {
    var retv37 int32
    var t38 int32 = a__1 + b__2
    retv37 = t38
    return retv37
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv40 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t41 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv40 = t41
    return retv40
}

func show(res__6 Result__int32__string) string {
    var retv43 string
    var jp45 string
    switch res__6.(type) {
    case Ok:
        var x25 int32 = res__6.(Ok)._0
        var value__7 int32 = x25
        var t46 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t47 string = "ok=" + t46
        jp45 = t47
    case Err:
        var x26 string = res__6.(Err)._0
        var err__8 string = x26
        var t48 string = "err=" + err__8
        jp45 = t48
    default:
        panic("non-exhaustive match")
    }
    retv43 = jp45
    return retv43
}

func main0() struct{} {
    var t50 Result__int32__string = plus_one(true)
    var t51 string = show(t50)
    println__T_string(t51)
    var t52 Result__int32__string = plus_one(false)
    var t53 string = show(t52)
    println__T_string(t53)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv55 string
    var t56 string = _goml_runtime_core_int32_to_string(self__2)
    retv55 = t56
    return retv55
}

func println__T_string(value__1 string) struct{} {
    var t58 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t58)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv61 string
    retv61 = self__9
    return retv61
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env29 closure_env_run_0) Result__int32__string {
    var retv63 Result__int32__string
    var flag__3 bool = env29.flag_0
    var mtmp22 Result__int32__string = parse_flag(flag__3)
    var jp65 int32
    switch mtmp22.(type) {
    case Ok:
        var x23 int32 = mtmp22.(Ok)._0
        var try_value__15 int32 = x23
        jp65 = try_value__15
        var value__4 int32 = jp65
        var t66 int32 = add(value__4, 1)
        var t67 Result__int32__string = Ok{
            _0: t66,
        }
        retv63 = t67
        return retv63
    case Err:
        var x24 string = mtmp22.(Err)._0
        var try_residual__15 string = x24
        var t68 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv63 = t68
        return retv63
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
