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

func parse(flag__0 bool) Result__int32__string {
    var retv73 Result__int32__string
    var jp75 Result__int32__string
    if flag__0 {
        var t76 Result__int32__string = Ok{
            _0: 5,
        }
        jp75 = t76
    } else {
        var t77 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp75 = t77
    }
    retv73 = jp75
    return retv73
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv79 Result__int32__string
    var jp81 int32
    if flag__1 {
        var mtmp64 Result__int32__string = parse(fallback__2)
        var jp85 int32
        switch mtmp64.(type) {
        case Ok:
            var x65 int32 = mtmp64.(Ok)._0
            var try_value__13 int32 = x65
            jp85 = try_value__13
            jp81 = jp85
            var value__3 int32 = jp81
            var t82 int32 = value__3 + 1
            var t83 Result__int32__string = Ok{
                _0: t82,
            }
            retv79 = t83
            return retv79
        case Err:
            var x66 string = mtmp64.(Err)._0
            var try_residual__13 string = x66
            var t86 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv79 = t86
            return retv79
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp81 = 10
        var value__3 int32 = jp81
        var t82 int32 = value__3 + 1
        var t83 Result__int32__string = Ok{
            _0: t82,
        }
        retv79 = t83
        return retv79
    }
}

func show(res__4 Result__int32__string) string {
    var retv88 string
    var jp90 string
    switch res__4.(type) {
    case Ok:
        var x67 int32 = res__4.(Ok)._0
        var value__5 int32 = x67
        var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t92 string = "ok=" + t91
        jp90 = t92
    case Err:
        var x68 string = res__4.(Err)._0
        var err__6 string = x68
        var t93 string = "err=" + err__6
        jp90 = t93
    default:
        panic("non-exhaustive match")
    }
    retv88 = jp90
    return retv88
}

func main0() struct{} {
    var t95 Result__int32__string = bump(true, true)
    var t96 string = show(t95)
    println__T_string(t96)
    var t97 Result__int32__string = bump(true, false)
    var t98 string = show(t97)
    println__T_string(t98)
    var t99 Result__int32__string = bump(false, false)
    var t100 string = show(t99)
    println__T_string(t100)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv102 string
    var t103 string = _goml_runtime_core_int32_to_string(self__6)
    retv102 = t103
    return retv102
}

func println__T_string(value__1 string) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t105)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv108 string
    retv108 = self__38
    return retv108
}

func main() {
    main0()
}
