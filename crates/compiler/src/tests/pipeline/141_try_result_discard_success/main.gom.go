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
    var retv67 Result__string__string
    var jp69 Result__string__string
    if ok__0 {
        var t70 Result__string__string = Ok{
            _0: "ignored",
        }
        jp69 = t70
    } else {
        var t71 Result__string__string = Err{
            _0: "parse failed",
        }
        jp69 = t71
    }
    retv67 = jp69
    return retv67
}

func check(ok__1 bool) Result__string__string {
    var retv73 Result__string__string
    var mtmp58 Result__string__string = parse_text(ok__1)
    switch mtmp58.(type) {
    case Ok:
        var t76 Result__string__string = Ok{
            _0: "ok",
        }
        retv73 = t76
        return retv73
    case Err:
        var x60 string = mtmp58.(Err)._0
        var try_residual__12 string = x60
        var t77 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv73 = t77
        return retv73
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv79 string
    var jp81 string
    switch res__2.(type) {
    case Ok:
        var x62 string = res__2.(Ok)._0
        var value__3 string = x62
        var t82 string = "ok " + value__3
        jp81 = t82
    case Err:
        var x63 string = res__2.(Err)._0
        var err__4 string = x63
        var t83 string = "err " + err__4
        jp81 = t83
    default:
        panic("non-exhaustive match")
    }
    retv79 = jp81
    return retv79
}

func main0() struct{} {
    var t85 Result__string__string = check(true)
    var t86 string = show(t85)
    println__T_string(t86)
    var t87 Result__string__string = check(false)
    var t88 string = show(t87)
    println__T_string(t88)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv93 string
    retv93 = self__34
    return retv93
}

func main() {
    main0()
}
