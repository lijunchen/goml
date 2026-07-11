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
    var retv16 Result__unit__string
    var jp18 Result__unit__string
    if ok__0 {
        var t19 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp18 = t19
    } else {
        var t20 Result__unit__string = Err{
            _0: "step failed",
        }
        jp18 = t20
    }
    retv16 = jp18
    return retv16
}

func run(ok__1 bool) Result__unit__string {
    var retv22 Result__unit__string
    var mtmp7 Result__unit__string = step(ok__1)
    switch mtmp7.(type) {
    case Ok:
        var t24 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv22 = t24
        return retv22
    case Err:
        var x9 string = mtmp7.(Err)._0
        var try_residual__12 string = x9
        var t25 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv22 = t25
        return retv22
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv27 string
    var jp29 string
    switch res__2.(type) {
    case Ok:
        var jp31 string
        jp31 = "ok unit"
        jp29 = jp31
    case Err:
        var x12 string = res__2.(Err)._0
        var err__3 string = x12
        var t32 string = "err " + err__3
        jp29 = t32
    default:
        panic("non-exhaustive match")
    }
    retv27 = jp29
    return retv27
}

func main0() struct{} {
    var t34 Result__unit__string = run(true)
    var t35 string = show(t34)
    println__T_string(t35)
    var t36 Result__unit__string = run(false)
    var t37 string = show(t36)
    println__T_string(t37)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t39 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t39)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv42 string
    retv42 = self__9
    return retv42
}

func main() {
    main0()
}
