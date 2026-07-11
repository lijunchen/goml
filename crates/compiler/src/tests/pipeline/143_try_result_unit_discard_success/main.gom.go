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
    var retv13 Result__unit__string
    var jp15 Result__unit__string
    if ok__0 {
        var t16 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp15 = t16
    } else {
        var t17 Result__unit__string = Err{
            _0: "step failed",
        }
        jp15 = t17
    }
    retv13 = jp15
    return retv13
}

func run(ok__1 bool) Result__unit__string {
    var retv19 Result__unit__string
    var mtmp4 Result__unit__string = step(ok__1)
    switch mtmp4.(type) {
    case Ok:
        var t21 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv19 = t21
        return retv19
    case Err:
        var x6 string = mtmp4.(Err)._0
        var try_residual__12 string = x6
        var t22 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv19 = t22
        return retv19
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv24 string
    var jp26 string
    switch res__2.(type) {
    case Ok:
        var jp28 string
        jp28 = "ok unit"
        jp26 = jp28
    case Err:
        var x9 string = res__2.(Err)._0
        var err__3 string = x9
        var t29 string = "err " + err__3
        jp26 = t29
    default:
        panic("non-exhaustive match")
    }
    retv24 = jp26
    return retv24
}

func main0() struct{} {
    var t31 Result__unit__string = run(true)
    var t32 string = show(t31)
    println__T_string(t32)
    var t33 Result__unit__string = run(false)
    var t34 string = show(t33)
    println__T_string(t34)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
