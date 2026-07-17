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
    var retv70 Result__string__string
    var jp72 Result__string__string
    if ok__0 {
        var t73 Result__string__string = Ok{
            _0: "ignored",
        }
        jp72 = t73
    } else {
        var t74 Result__string__string = Err{
            _0: "parse failed",
        }
        jp72 = t74
    }
    retv70 = jp72
    return retv70
}

func check(ok__1 bool) Result__string__string {
    var retv76 Result__string__string
    var mtmp61 Result__string__string = parse_text(ok__1)
    switch mtmp61.(type) {
    case Ok:
        var t79 Result__string__string = Ok{
            _0: "ok",
        }
        retv76 = t79
        return retv76
    case Err:
        var x63 string = mtmp61.(Err)._0
        var try_residual__12 string = x63
        var t80 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv76 = t80
        return retv76
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv82 string
    var jp84 string
    switch res__2.(type) {
    case Ok:
        var x65 string = res__2.(Ok)._0
        var value__3 string = x65
        var t85 string = "ok " + value__3
        jp84 = t85
    case Err:
        var x66 string = res__2.(Err)._0
        var err__4 string = x66
        var t86 string = "err " + err__4
        jp84 = t86
    default:
        panic("non-exhaustive match")
    }
    retv82 = jp84
    return retv82
}

func main0() struct{} {
    var t88 Result__string__string = check(true)
    var t89 string = show(t88)
    println__T_string(t89)
    var t90 Result__string__string = check(false)
    var t91 string = show(t90)
    println__T_string(t91)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv96 string
    retv96 = self__37
    return retv96
}

func main() {
    main0()
}
