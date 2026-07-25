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

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

func parse(flag__0 bool) Result__int32__string {
    var retv71 Result__int32__string
    var jp73 Result__int32__string
    if flag__0 {
        var t74 Result__int32__string = Ok{
            _0: 41,
        }
        jp73 = t74
    } else {
        var t75 Result__int32__string = Err{
            _0: "bad",
        }
        jp73 = t75
    }
    retv71 = jp73
    return retv71
}

func compute(flag__1 bool) Result__int32__string {
    var retv77 Result__int32__string
    var mtmp64 Result__int32__string = parse(flag__1)
    var jp79 int32
    switch mtmp64.(type) {
    case Err:
        var x65 string = mtmp64.(Err)._0
        var try_residual__12 string = x65
        var t82 Result__int32__string = Err{
            _0: try_residual__12,
        }
        retv77 = t82
        return retv77
    case Ok:
        var x66 int32 = mtmp64.(Ok)._0
        var try_value__12 int32 = x66
        jp79 = try_value__12
        var value__2 int32 = jp79
        var t80 int32 = value__2 + 1
        var t81 Result__int32__string = Ok{
            _0: t80,
        }
        retv77 = t81
        return retv77
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Result__int32__string) string {
    var retv84 string
    var jp86 string
    switch value__3.(type) {
    case Err:
        var x67 string = value__3.(Err)._0
        var error__5 string = x67
        jp86 = error__5
    case Ok:
        var x68 int32 = value__3.(Ok)._0
        var value__4 int32 = x68
        var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp86 = t87
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func main0() struct{} {
    var t89 Result__int32__string = compute(true)
    var t90 string = show(t89)
    println__T_string(t90)
    var t91 Result__int32__string = compute(false)
    var t92 string = show(t91)
    println__T_string(t92)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv95 string
    var t96 string = _goml_runtime_core_int32_to_string(self__6)
    retv95 = t96
    return retv95
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv101 string
    retv101 = self__38
    return retv101
}

func main() {
    main0()
}
