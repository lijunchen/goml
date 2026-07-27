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
    var retv73 Result__string__string
    var jp75 Result__string__string
    if ok__0 {
        var t76 Result__string__string = Ok{
            _0: "ignored",
        }
        jp75 = t76
    } else {
        var t77 Result__string__string = Err{
            _0: "parse failed",
        }
        jp75 = t77
    }
    retv73 = jp75
    return retv73
}

func check(ok__1 bool) Result__string__string {
    var retv79 Result__string__string
    var mtmp64 Result__string__string = parse_text(ok__1)
    switch mtmp64.(type) {
    case Ok:
        var t82 Result__string__string = Ok{
            _0: "ok",
        }
        retv79 = t82
        return retv79
    case Err:
        var x66 string = mtmp64.(Err)._0
        var try_residual__12 string = x66
        var t83 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv79 = t83
        return retv79
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv85 string
    var jp87 string
    switch res__2.(type) {
    case Ok:
        var x68 string = res__2.(Ok)._0
        var value__3 string = x68
        var t88 string = "ok " + value__3
        jp87 = t88
    case Err:
        var x69 string = res__2.(Err)._0
        var err__4 string = x69
        var t89 string = "err " + err__4
        jp87 = t89
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func main0() struct{} {
    var t91 Result__string__string = check(true)
    var t92 string = show(t91)
    println__T_string(t92)
    var t93 Result__string__string = check(false)
    var t94 string = show(t93)
    println__T_string(t94)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv99 string
    retv99 = self__38
    return retv99
}

func main() {
    main0()
}
