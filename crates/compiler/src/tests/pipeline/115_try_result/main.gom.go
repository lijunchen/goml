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
    var retv70 Result__int32__string
    var jp72 Result__int32__string
    if flag__0 {
        var t73 Result__int32__string = Ok{
            _0: 7,
        }
        jp72 = t73
    } else {
        var t74 Result__int32__string = Err{
            _0: "nope",
        }
        jp72 = t74
    }
    retv70 = jp72
    return retv70
}

func add(a__1 int32, b__2 int32) int32 {
    var retv76 int32
    var t77 int32 = a__1 + b__2
    retv76 = t77
    return retv76
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv79 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t80 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv79 = t80
    return retv79
}

func show(res__6 Result__int32__string) string {
    var retv82 string
    var jp84 string
    switch res__6.(type) {
    case Ok:
        var x64 int32 = res__6.(Ok)._0
        var value__7 int32 = x64
        var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t86 string = "ok=" + t85
        jp84 = t86
    case Err:
        var x65 string = res__6.(Err)._0
        var err__8 string = x65
        var t87 string = "err=" + err__8
        jp84 = t87
    default:
        panic("non-exhaustive match")
    }
    retv82 = jp84
    return retv82
}

func main0() struct{} {
    var t89 Result__int32__string = plus_one(true)
    var t90 string = show(t89)
    println__T_string(t90)
    var t91 Result__int32__string = plus_one(false)
    var t92 string = show(t91)
    println__T_string(t92)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__5)
    retv94 = t95
    return retv94
}

func println__T_string(value__1 string) struct{} {
    var t97 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t97)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv100 string
    retv100 = self__37
    return retv100
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env68 closure_env_run_0) Result__int32__string {
    var retv102 Result__int32__string
    var flag__3 bool = env68.flag_0
    var mtmp61 Result__int32__string = parse_flag(flag__3)
    var jp104 int32
    switch mtmp61.(type) {
    case Ok:
        var x62 int32 = mtmp61.(Ok)._0
        var try_value__15 int32 = x62
        jp104 = try_value__15
        var value__4 int32 = jp104
        var t105 int32 = add(value__4, 1)
        var t106 Result__int32__string = Ok{
            _0: t105,
        }
        retv102 = t106
        return retv102
    case Err:
        var x63 string = mtmp61.(Err)._0
        var try_residual__15 string = x63
        var t107 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv102 = t107
        return retv102
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
