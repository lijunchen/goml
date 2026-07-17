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
    var retv72 Result__string__string
    var jp74 Result__string__string
    if ok__0 {
        var t75 Result__string__string = Ok{
            _0: "goml",
        }
        jp74 = t75
    } else {
        var t76 Result__string__string = Err{
            _0: "parse failed",
        }
        jp74 = t76
    }
    retv72 = jp74
    return retv72
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv78 Result__string__string
    var mtmp61 Result__string__string = parse_text(ok__1)
    var jp80 string
    switch mtmp61.(type) {
    case Ok:
        var x62 string = mtmp61.(Ok)._0
        var try_value__12 string = x62
        jp80 = try_value__12
        var text__2 string = jp80
        var t81 string = text__2 + "!"
        var t82 Result__string__string = Ok{
            _0: t81,
        }
        retv78 = t82
        return retv78
    case Err:
        var x63 string = mtmp61.(Err)._0
        var try_residual__12 string = x63
        var t83 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv78 = t83
        return retv78
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv85 Result__string__string
    var mtmp64 Result__string__string = normalize_text(ok__3)
    var jp87 string
    switch mtmp64.(type) {
    case Ok:
        var x65 string = mtmp64.(Ok)._0
        var try_value__20 string = x65
        jp87 = try_value__20
        var text__4 string = jp87
        var t88 string = "[" + text__4
        var t89 string = t88 + "]"
        var t90 Result__string__string = Ok{
            _0: t89,
        }
        retv85 = t90
        return retv85
    case Err:
        var x66 string = mtmp64.(Err)._0
        var try_residual__20 string = x66
        var t91 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv85 = t91
        return retv85
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv93 string
    var jp95 string
    switch res__5.(type) {
    case Ok:
        var x67 string = res__5.(Ok)._0
        var value__6 string = x67
        var t96 string = "ok " + value__6
        jp95 = t96
    case Err:
        var x68 string = res__5.(Err)._0
        var err__7 string = x68
        var t97 string = "err " + err__7
        jp95 = t97
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func main0() struct{} {
    var t99 Result__string__string = decorate_text(true)
    var t100 string = show(t99)
    println__T_string(t100)
    var t101 Result__string__string = decorate_text(false)
    var t102 string = show(t101)
    println__T_string(t102)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv107 string
    retv107 = self__37
    return retv107
}

func main() {
    main0()
}
