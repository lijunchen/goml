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
    var retv79 Result__string__string
    var jp81 Result__string__string
    if ok__0 {
        var t82 Result__string__string = Ok{
            _0: "goml",
        }
        jp81 = t82
    } else {
        var t83 Result__string__string = Err{
            _0: "parse failed",
        }
        jp81 = t83
    }
    retv79 = jp81
    return retv79
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv85 Result__string__string
    var mtmp68 Result__string__string = parse_text(ok__1)
    var jp87 string
    switch mtmp68.(type) {
    case Ok:
        var x69 string = mtmp68.(Ok)._0
        var try_value__12 string = x69
        jp87 = try_value__12
        var text__2 string = jp87
        var t88 string = text__2 + "!"
        var t89 Result__string__string = Ok{
            _0: t88,
        }
        retv85 = t89
        return retv85
    case Err:
        var x70 string = mtmp68.(Err)._0
        var try_residual__12 string = x70
        var t90 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv85 = t90
        return retv85
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv92 Result__string__string
    var mtmp71 Result__string__string = normalize_text(ok__3)
    var jp94 string
    switch mtmp71.(type) {
    case Ok:
        var x72 string = mtmp71.(Ok)._0
        var try_value__20 string = x72
        jp94 = try_value__20
        var text__4 string = jp94
        var t95 string = "[" + text__4
        var t96 string = t95 + "]"
        var t97 Result__string__string = Ok{
            _0: t96,
        }
        retv92 = t97
        return retv92
    case Err:
        var x73 string = mtmp71.(Err)._0
        var try_residual__20 string = x73
        var t98 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv92 = t98
        return retv92
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv100 string
    var jp102 string
    switch res__5.(type) {
    case Ok:
        var x74 string = res__5.(Ok)._0
        var value__6 string = x74
        var t103 string = "ok " + value__6
        jp102 = t103
    case Err:
        var x75 string = res__5.(Err)._0
        var err__7 string = x75
        var t104 string = "err " + err__7
        jp102 = t104
    default:
        panic("non-exhaustive match")
    }
    retv100 = jp102
    return retv100
}

func main0() struct{} {
    var t106 Result__string__string = decorate_text(true)
    var t107 string = show(t106)
    println__T_string(t107)
    var t108 Result__string__string = decorate_text(false)
    var t109 string = show(t108)
    println__T_string(t109)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t111 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t111)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv114 string
    retv114 = self__38
    return retv114
}

func main() {
    main0()
}
