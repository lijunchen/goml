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
    var retv117 Result__int32__string
    var jp119 Result__int32__string
    if flag__0 {
        var t120 Result__int32__string = Ok{
            _0: 7,
        }
        jp119 = t120
    } else {
        var t121 Result__int32__string = Err{
            _0: "nope",
        }
        jp119 = t121
    }
    retv117 = jp119
    return retv117
}

func add(a__1 int32, b__2 int32) int32 {
    var retv123 int32
    var t124 int32 = a__1 + b__2
    retv123 = t124
    return retv123
}

func plus_one(flag__3 bool) Result__int32__string {
    var retv126 Result__int32__string
    var run__5 closure_env_run_0 = closure_env_run_0{
        flag_0: flag__3,
    }
    var t127 Result__int32__string = _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(run__5)
    retv126 = t127
    return retv126
}

func show(res__6 Result__int32__string) string {
    var retv129 string
    var jp131 string
    switch res__6.(type) {
    case Ok:
        var x111 int32 = res__6.(Ok)._0
        var value__7 int32 = x111
        var t132 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__7)
        var t133 string = "ok=" + t132
        jp131 = t133
    case Err:
        var x112 string = res__6.(Err)._0
        var err__8 string = x112
        var t134 string = "err=" + err__8
        jp131 = t134
    default:
        panic("non-exhaustive match")
    }
    retv129 = jp131
    return retv129
}

func main0() struct{} {
    var t136 Result__int32__string = plus_one(true)
    var t137 string = show(t136)
    println__T_string(t137)
    var t138 Result__int32__string = plus_one(false)
    var t139 string = show(t138)
    println__T_string(t139)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv141 string
    var t142 string = _goml_runtime_core_int32_to_string(self__6)
    retv141 = t142
    return retv141
}

func println__T_string(value__1 string) struct{} {
    var t144 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t144)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv147 string
    retv147 = self__38
    return retv147
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env115 closure_env_run_0) Result__int32__string {
    var retv149 Result__int32__string
    var flag__3 bool = env115.flag_0
    var mtmp108 Result__int32__string = parse_flag(flag__3)
    var jp151 int32
    switch mtmp108.(type) {
    case Ok:
        var x109 int32 = mtmp108.(Ok)._0
        var try_value__15 int32 = x109
        jp151 = try_value__15
        var value__4 int32 = jp151
        var t152 int32 = add(value__4, 1)
        var t153 Result__int32__string = Ok{
            _0: t152,
        }
        retv149 = t153
        return retv149
    case Err:
        var x110 string = mtmp108.(Err)._0
        var try_residual__15 string = x110
        var t154 Result__int32__string = Err{
            _0: try_residual__15,
        }
        retv149 = t154
        return retv149
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
