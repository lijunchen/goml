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
    var retv73 Result__int32__string
    var jp75 Result__int32__string
    if flag__0 {
        var t76 Result__int32__string = Ok{
            _0: 7,
        }
        jp75 = t76
    } else {
        var t77 Result__int32__string = Err{
            _0: "nope",
        }
        jp75 = t77
    }
    retv73 = jp75
    return retv73
}

func add(a__1 int32, b__2 int32) int32 {
    var retv79 int32
    var t80 int32 = a__1 + b__2
    retv79 = t80
    return retv79
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv82 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t83 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv82 = t83
    return retv82
}

func show(res__6 Result__int32__string) string {
    var retv85 string
    var jp87 string
    switch res__6.(type) {
    case Ok:
        var x67 int32 = res__6.(Ok)._0
        var value__7 int32 = x67
        var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t89 string = "ok=" + t88
        jp87 = t89
    case Err:
        var x68 string = res__6.(Err)._0
        var err__8 string = x68
        var t90 string = "err=" + err__8
        jp87 = t90
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func main0() struct{} {
    var t92 Result__int32__string = plus_one(true)
    var t93 string = show(t92)
    println__T_string(t93)
    var t94 Result__int32__string = plus_one(false)
    var t95 string = show(t94)
    println__T_string(t95)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int32_to_string(self__6)
    retv97 = t98
    return retv97
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv103 string
    retv103 = self__38
    return retv103
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env71 closure_env_run_0) Result__int32__string {
    var retv105 Result__int32__string
    var flag__3 bool = env71.flag_0
    var mtmp64 Result__int32__string = parse_flag(flag__3)
    var jp107 int32
    switch mtmp64.(type) {
    case Ok:
        var x65 int32 = mtmp64.(Ok)._0
        var try_value__15 int32 = x65
        jp107 = try_value__15
        var value__4 int32 = jp107
        var t108 int32 = add(value__4, 1)
        var t109 Result__int32__string = Ok{
            _0: t108,
        }
        retv105 = t109
        return retv105
    case Err:
        var x66 string = mtmp64.(Err)._0
        var try_residual__15 string = x66
        var t110 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv105 = t110
        return retv105
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
