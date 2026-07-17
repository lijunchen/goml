package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Result__unit__string interface {
    isResult__unit__string()
}

type Ok struct {
    _0 struct{}
}

func (_ Ok) isResult__unit__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__unit__string() {}

func step(ok__0 bool) Result__unit__string {
    var retv67 Result__unit__string
    var jp69 Result__unit__string
    if ok__0 {
        var t70 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp69 = t70
    } else {
        var t71 Result__unit__string = Err{
            _0: "step failed",
        }
        jp69 = t71
    }
    retv67 = jp69
    return retv67
}

func run(ok__1 bool) Result__unit__string {
    var retv73 Result__unit__string
    var mtmp58 Result__unit__string = step(ok__1)
    switch mtmp58.(type) {
    case Ok:
        var t75 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv73 = t75
        return retv73
    case Err:
        var x60 string = mtmp58.(Err)._0
        var try_residual__12 string = x60
        var t76 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv73 = t76
        return retv73
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv78 string
    var jp80 string
    switch res__2.(type) {
    case Ok:
        var jp82 string
        jp82 = "ok unit"
        jp80 = jp82
    case Err:
        var x63 string = res__2.(Err)._0
        var err__3 string = x63
        var t83 string = "err " + err__3
        jp80 = t83
    default:
        panic("non-exhaustive match")
    }
    retv78 = jp80
    return retv78
}

func main0() struct{} {
    var t85 Result__unit__string = run(true)
    var t86 string = show(t85)
    println__T_string(t86)
    var t87 Result__unit__string = run(false)
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
