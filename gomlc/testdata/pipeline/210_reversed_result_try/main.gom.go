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
    var retv75 Result__int32__string
    var jp77 Result__int32__string
    if flag__0 {
        var t78 Result__int32__string = Ok{
            _0: 41,
        }
        jp77 = t78
    } else {
        var t79 Result__int32__string = Err{
            _0: "bad",
        }
        jp77 = t79
    }
    retv75 = jp77
    return retv75
}

func compute(flag__1 bool) Result__int32__string {
    var retv81 Result__int32__string
    var mtmp68 Result__int32__string = parse(flag__1)
    var jp83 int32
    switch mtmp68.(type) {
    case Err:
        var x69 string = mtmp68.(Err)._0
        var try_residual__12 string = x69
        var t86 Result__int32__string = Err{
            _0: try_residual__12,
        }
        retv81 = t86
        return retv81
    case Ok:
        var x70 int32 = mtmp68.(Ok)._0
        var try_value__12 int32 = x70
        jp83 = try_value__12
        var value__2 int32 = jp83
        var t84 int32 = value__2 + 1
        var t85 Result__int32__string = Ok{
            _0: t84,
        }
        retv81 = t85
        return retv81
    default:
        panic("non-exhaustive match")
    }
}

func show(value__3 Result__int32__string) string {
    var retv88 string
    var jp90 string
    switch value__3.(type) {
    case Err:
        var x71 string = value__3.(Err)._0
        var error__5 string = x71
        jp90 = error__5
    case Ok:
        var x72 int32 = value__3.(Ok)._0
        var value__4 int32 = x72
        var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        jp90 = t91
    default:
        panic("non-exhaustive match")
    }
    retv88 = jp90
    return retv88
}

func main0() struct{} {
    var t93 Result__int32__string = compute(true)
    var t94 string = show(t93)
    println__T_string(t94)
    var t95 Result__int32__string = compute(false)
    var t96 string = show(t95)
    println__T_string(t96)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv99 string
    var t100 string = _goml_runtime_core_int32_to_string(self__6)
    retv99 = t100
    return retv99
}

func println__T_string(value__1 string) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv105 string
    retv105 = self__38
    return retv105
}

func main() {
    main0()
}
