package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func parse_text(ok__0 bool) Result__string__string {
    var retv69 Result__string__string
    var jp71 Result__string__string
    if ok__0 {
        var t72 Result__string__string = Ok{
            _0: "goml",
        }
        jp71 = t72
    } else {
        var t73 Result__string__string = Err{
            _0: "parse failed",
        }
        jp71 = t73
    }
    retv69 = jp71
    return retv69
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv75 Result__string__string
    var mtmp58 Result__string__string = parse_text(ok__1)
    var jp77 string
    switch mtmp58.(type) {
    case Ok:
        var x59 string = mtmp58.(Ok)._0
        var try_value__12 string = x59
        jp77 = try_value__12
        var text__2 string = jp77
        var t78 string = text__2 + "!"
        var t79 Result__string__string = Ok{
            _0: t78,
        }
        retv75 = t79
        return retv75
    case Err:
        var x60 string = mtmp58.(Err)._0
        var try_residual__12 string = x60
        var t80 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv75 = t80
        return retv75
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv82 Result__string__string
    var mtmp61 Result__string__string = normalize_text(ok__3)
    var jp84 string
    switch mtmp61.(type) {
    case Ok:
        var x62 string = mtmp61.(Ok)._0
        var try_value__20 string = x62
        jp84 = try_value__20
        var text__4 string = jp84
        var t85 string = "[" + text__4
        var t86 string = t85 + "]"
        var t87 Result__string__string = Ok{
            _0: t86,
        }
        retv82 = t87
        return retv82
    case Err:
        var x63 string = mtmp61.(Err)._0
        var try_residual__20 string = x63
        var t88 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv82 = t88
        return retv82
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv90 string
    var jp92 string
    switch res__5.(type) {
    case Ok:
        var x64 string = res__5.(Ok)._0
        var value__6 string = x64
        var t93 string = "ok " + value__6
        jp92 = t93
    case Err:
        var x65 string = res__5.(Err)._0
        var err__7 string = x65
        var t94 string = "err " + err__7
        jp92 = t94
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func main0() struct{} {
    var t96 Result__string__string = decorate_text(true)
    var t97 string = show(t96)
    println__T_string(t97)
    var t98 Result__string__string = decorate_text(false)
    var t99 string = show(t98)
    println__T_string(t99)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv104 string
    retv104 = self__34
    return retv104
}

func main() {
    main0()
}
