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
    var retv67 Result__int32__string
    var jp69 Result__int32__string
    if flag__0 {
        var t70 Result__int32__string = Ok{
            _0: 5,
        }
        jp69 = t70
    } else {
        var t71 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp69 = t71
    }
    retv67 = jp69
    return retv67
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv73 Result__int32__string
    var jp75 int32
    if flag__1 {
        var mtmp58 Result__int32__string = parse(fallback__2)
        var jp79 int32
        switch mtmp58.(type) {
        case Ok:
            var x59 int32 = mtmp58.(Ok)._0
            var try_value__13 int32 = x59
            jp79 = try_value__13
            jp75 = jp79
            var value__3 int32 = jp75
            var t76 int32 = value__3 + 1
            var t77 Result__int32__string = Ok{
                _0: t76,
            }
            retv73 = t77
            return retv73
        case Err:
            var x60 string = mtmp58.(Err)._0
            var try_residual__13 string = x60
            var t80 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv73 = t80
            return retv73
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp75 = 10
        var value__3 int32 = jp75
        var t76 int32 = value__3 + 1
        var t77 Result__int32__string = Ok{
            _0: t76,
        }
        retv73 = t77
        return retv73
    }
}

func show(res__4 Result__int32__string) string {
    var retv82 string
    var jp84 string
    switch res__4.(type) {
    case Ok:
        var x61 int32 = res__4.(Ok)._0
        var value__5 int32 = x61
        var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t86 string = "ok=" + t85
        jp84 = t86
    case Err:
        var x62 string = res__4.(Err)._0
        var err__6 string = x62
        var t87 string = "err=" + err__6
        jp84 = t87
    default:
        panic("non-exhaustive match")
    }
    retv82 = jp84
    return retv82
}

func main0() struct{} {
    var t89 Result__int32__string = bump(true, true)
    var t90 string = show(t89)
    println__T_string(t90)
    var t91 Result__int32__string = bump(true, false)
    var t92 string = show(t91)
    println__T_string(t92)
    var t93 Result__int32__string = bump(false, false)
    var t94 string = show(t93)
    println__T_string(t94)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv96 string
    var t97 string = _goml_runtime_core_int32_to_string(self__2)
    retv96 = t97
    return retv96
}

func println__T_string(value__1 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv102 string
    retv102 = self__34
    return retv102
}

func main() {
    main0()
}
