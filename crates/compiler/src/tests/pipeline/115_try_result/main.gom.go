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
    var retv67 Result__int32__string
    var jp69 Result__int32__string
    if flag__0 {
        var t70 Result__int32__string = Ok{
            _0: 7,
        }
        jp69 = t70
    } else {
        var t71 Result__int32__string = Err{
            _0: "nope",
        }
        jp69 = t71
    }
    retv67 = jp69
    return retv67
}

func add(a__1 int32, b__2 int32) int32 {
    var retv73 int32
    var t74 int32 = a__1 + b__2
    retv73 = t74
    return retv73
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv76 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t77 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv76 = t77
    return retv76
}

func show(res__6 Result__int32__string) string {
    var retv79 string
    var jp81 string
    switch res__6.(type) {
    case Ok:
        var x61 int32 = res__6.(Ok)._0
        var value__7 int32 = x61
        var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t83 string = "ok=" + t82
        jp81 = t83
    case Err:
        var x62 string = res__6.(Err)._0
        var err__8 string = x62
        var t84 string = "err=" + err__8
        jp81 = t84
    default:
        panic("non-exhaustive match")
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var t86 Result__int32__string = plus_one(true)
    var t87 string = show(t86)
    println__T_string(t87)
    var t88 Result__int32__string = plus_one(false)
    var t89 string = show(t88)
    println__T_string(t89)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv91 string
    var t92 string = _goml_runtime_core_int32_to_string(self__2)
    retv91 = t92
    return retv91
}

func println__T_string(value__1 string) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv97 string
    retv97 = self__34
    return retv97
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env65 closure_env_run_0) Result__int32__string {
    var retv99 Result__int32__string
    var flag__3 bool = env65.flag_0
    var mtmp58 Result__int32__string = parse_flag(flag__3)
    var jp101 int32
    switch mtmp58.(type) {
    case Ok:
        var x59 int32 = mtmp58.(Ok)._0
        var try_value__15 int32 = x59
        jp101 = try_value__15
        var value__4 int32 = jp101
        var t102 int32 = add(value__4, 1)
        var t103 Result__int32__string = Ok{
            _0: t102,
        }
        retv99 = t103
        return retv99
    case Err:
        var x60 string = mtmp58.(Err)._0
        var try_residual__15 string = x60
        var t104 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv99 = t104
        return retv99
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
