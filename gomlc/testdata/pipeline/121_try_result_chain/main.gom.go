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
    var retv75 Result__string__string
    var jp77 Result__string__string
    if ok__0 {
        var t78 Result__string__string = Ok{
            _0: "goml",
        }
        jp77 = t78
    } else {
        var t79 Result__string__string = Err{
            _0: "parse failed",
        }
        jp77 = t79
    }
    retv75 = jp77
    return retv75
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv81 Result__string__string
    var mtmp64 Result__string__string = parse_text(ok__1)
    var jp83 string
    switch mtmp64.(type) {
    case Ok:
        var x65 string = mtmp64.(Ok)._0
        var try_value__12 string = x65
        jp83 = try_value__12
        var text__2 string = jp83
        var t84 string = text__2 + "!"
        var t85 Result__string__string = Ok{
            _0: t84,
        }
        retv81 = t85
        return retv81
    case Err:
        var x66 string = mtmp64.(Err)._0
        var try_residual__12 string = x66
        var t86 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv81 = t86
        return retv81
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv88 Result__string__string
    var mtmp67 Result__string__string = normalize_text(ok__3)
    var jp90 string
    switch mtmp67.(type) {
    case Ok:
        var x68 string = mtmp67.(Ok)._0
        var try_value__20 string = x68
        jp90 = try_value__20
        var text__4 string = jp90
        var t91 string = "[" + text__4
        var t92 string = t91 + "]"
        var t93 Result__string__string = Ok{
            _0: t92,
        }
        retv88 = t93
        return retv88
    case Err:
        var x69 string = mtmp67.(Err)._0
        var try_residual__20 string = x69
        var t94 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv88 = t94
        return retv88
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv96 string
    var jp98 string
    switch res__5.(type) {
    case Ok:
        var x70 string = res__5.(Ok)._0
        var value__6 string = x70
        var t99 string = "ok " + value__6
        jp98 = t99
    case Err:
        var x71 string = res__5.(Err)._0
        var err__7 string = x71
        var t100 string = "err " + err__7
        jp98 = t100
    default:
        panic("non-exhaustive match")
    }
    retv96 = jp98
    return retv96
}

func main0() struct{} {
    var t102 Result__string__string = decorate_text(true)
    var t103 string = show(t102)
    println__T_string(t103)
    var t104 Result__string__string = decorate_text(false)
    var t105 string = show(t104)
    println__T_string(t105)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t107 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t107)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv110 string
    retv110 = self__38
    return retv110
}

func main() {
    main0()
}
