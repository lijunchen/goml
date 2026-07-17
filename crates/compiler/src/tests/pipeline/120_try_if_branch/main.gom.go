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
    var retv70 Result__int32__string
    var jp72 Result__int32__string
    if flag__0 {
        var t73 Result__int32__string = Ok{
            _0: 5,
        }
        jp72 = t73
    } else {
        var t74 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp72 = t74
    }
    retv70 = jp72
    return retv70
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv76 Result__int32__string
    var jp78 int32
    if flag__1 {
        var mtmp61 Result__int32__string = parse(fallback__2)
        var jp82 int32
        switch mtmp61.(type) {
        case Ok:
            var x62 int32 = mtmp61.(Ok)._0
            var try_value__13 int32 = x62
            jp82 = try_value__13
            jp78 = jp82
            var value__3 int32 = jp78
            var t79 int32 = value__3 + 1
            var t80 Result__int32__string = Ok{
                _0: t79,
            }
            retv76 = t80
            return retv76
        case Err:
            var x63 string = mtmp61.(Err)._0
            var try_residual__13 string = x63
            var t83 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv76 = t83
            return retv76
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp78 = 10
        var value__3 int32 = jp78
        var t79 int32 = value__3 + 1
        var t80 Result__int32__string = Ok{
            _0: t79,
        }
        retv76 = t80
        return retv76
    }
}

func show(res__4 Result__int32__string) string {
    var retv85 string
    var jp87 string
    switch res__4.(type) {
    case Ok:
        var x64 int32 = res__4.(Ok)._0
        var value__5 int32 = x64
        var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__5)
        var t89 string = "ok=" + t88
        jp87 = t89
    case Err:
        var x65 string = res__4.(Err)._0
        var err__6 string = x65
        var t90 string = "err=" + err__6
        jp87 = t90
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func main0() struct{} {
    var t92 Result__int32__string = bump(true, true)
    var t93 string = show(t92)
    println__T_string(t93)
    var t94 Result__int32__string = bump(true, false)
    var t95 string = show(t94)
    println__T_string(t95)
    var t96 Result__int32__string = bump(false, false)
    var t97 string = show(t96)
    println__T_string(t97)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv99 string
    var t100 string = _goml_runtime_core_int32_to_string(self__5)
    retv99 = t100
    return retv99
}

func println__T_string(value__1 string) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv105 string
    retv105 = self__37
    return retv105
}

func main() {
    main0()
}
