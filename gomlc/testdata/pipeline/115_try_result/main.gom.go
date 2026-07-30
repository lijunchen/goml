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
    var retv77 Result__int32__string
    var jp79 Result__int32__string
    if flag__0 {
        var t80 Result__int32__string = Ok{
            _0: 7,
        }
        jp79 = t80
    } else {
        var t81 Result__int32__string = Err{
            _0: "nope",
        }
        jp79 = t81
    }
    retv77 = jp79
    return retv77
}

func add(a__1 int32, b__2 int32) int32 {
    var retv83 int32
    var t84 int32 = a__1 + b__2
    retv83 = t84
    return retv83
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv86 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t87 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv86 = t87
    return retv86
}

func show(res__6 Result__int32__string) string {
    var retv89 string
    var jp91 string
    switch res__6.(type) {
    case Ok:
        var x71 int32 = res__6.(Ok)._0
        var value__7 int32 = x71
        var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t93 string = "ok=" + t92
        jp91 = t93
    case Err:
        var x72 string = res__6.(Err)._0
        var err__8 string = x72
        var t94 string = "err=" + err__8
        jp91 = t94
    default:
        panic("non-exhaustive match")
    }
    retv89 = jp91
    return retv89
}

func main0() struct{} {
    var t96 Result__int32__string = plus_one(true)
    var t97 string = show(t96)
    println__T_string(t97)
    var t98 Result__int32__string = plus_one(false)
    var t99 string = show(t98)
    println__T_string(t99)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv101 string
    var t102 string = _goml_runtime_core_int32_to_string(self__6)
    retv101 = t102
    return retv101
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv107 string
    retv107 = self__38
    return retv107
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env75 closure_env_run_0) Result__int32__string {
    var retv109 Result__int32__string
    var flag__3 bool = env75.flag_0
    var mtmp68 Result__int32__string = parse_flag(flag__3)
    var jp111 int32
    switch mtmp68.(type) {
    case Ok:
        var x69 int32 = mtmp68.(Ok)._0
        var try_value__15 int32 = x69
        jp111 = try_value__15
        var value__4 int32 = jp111
        var t112 int32 = add(value__4, 1)
        var t113 Result__int32__string = Ok{
            _0: t112,
        }
        retv109 = t113
        return retv109
    case Err:
        var x70 string = mtmp68.(Err)._0
        var try_residual__15 string = x70
        var t114 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv109 = t114
        return retv109
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
