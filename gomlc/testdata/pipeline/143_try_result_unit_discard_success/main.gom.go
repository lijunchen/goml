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
    var retv77 Result__unit__string
    var jp79 Result__unit__string
    if ok__0 {
        var t80 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp79 = t80
    } else {
        var t81 Result__unit__string = Err{
            _0: "step failed",
        }
        jp79 = t81
    }
    retv77 = jp79
    return retv77
}

func run(ok__1 bool) Result__unit__string {
    var retv83 Result__unit__string
    var mtmp68 Result__unit__string = step(ok__1)
    switch mtmp68.(type) {
    case Ok:
        var t85 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv83 = t85
        return retv83
    case Err:
        var x70 string = mtmp68.(Err)._0
        var try_residual__12 string = x70
        var t86 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv83 = t86
        return retv83
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv88 string
    var jp90 string
    switch res__2.(type) {
    case Ok:
        var jp92 string
        jp92 = "ok unit"
        jp90 = jp92
    case Err:
        var x73 string = res__2.(Err)._0
        var err__3 string = x73
        var t93 string = "err " + err__3
        jp90 = t93
    default:
        panic("non-exhaustive match")
    }
    retv88 = jp90
    return retv88
}

func main0() struct{} {
    var t95 Result__unit__string = run(true)
    var t96 string = show(t95)
    println__T_string(t96)
    var t97 Result__unit__string = run(false)
    var t98 string = show(t97)
    println__T_string(t98)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv103 string
    retv103 = self__38
    return retv103
}

func main() {
    main0()
}
