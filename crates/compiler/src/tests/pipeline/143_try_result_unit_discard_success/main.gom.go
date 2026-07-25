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
    var retv73 Result__unit__string
    var jp75 Result__unit__string
    if ok__0 {
        var t76 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp75 = t76
    } else {
        var t77 Result__unit__string = Err{
            _0: "step failed",
        }
        jp75 = t77
    }
    retv73 = jp75
    return retv73
}

func run(ok__1 bool) Result__unit__string {
    var retv79 Result__unit__string
    var mtmp64 Result__unit__string = step(ok__1)
    switch mtmp64.(type) {
    case Ok:
        var t81 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv79 = t81
        return retv79
    case Err:
        var x66 string = mtmp64.(Err)._0
        var try_residual__12 string = x66
        var t82 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv79 = t82
        return retv79
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv84 string
    var jp86 string
    switch res__2.(type) {
    case Ok:
        var jp88 string
        jp88 = "ok unit"
        jp86 = jp88
    case Err:
        var x69 string = res__2.(Err)._0
        var err__3 string = x69
        var t89 string = "err " + err__3
        jp86 = t89
    default:
        panic("non-exhaustive match")
    }
    retv84 = jp86
    return retv84
}

func main0() struct{} {
    var t91 Result__unit__string = run(true)
    var t92 string = show(t91)
    println__T_string(t92)
    var t93 Result__unit__string = run(false)
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
