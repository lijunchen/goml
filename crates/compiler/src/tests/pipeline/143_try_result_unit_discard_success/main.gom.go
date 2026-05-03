package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
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
    var retv9 Result__unit__string
    var jp11 Result__unit__string
    if ok__0 {
        var t12 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp11 = t12
    } else {
        var t13 Result__unit__string = Err{
            _0: "step failed",
        }
        jp11 = t13
    }
    retv9 = jp11
    return retv9
}

func run(ok__1 bool) Result__unit__string {
    var retv15 Result__unit__string
    var mtmp0 Result__unit__string = step(ok__1)
    switch mtmp0.(type) {
    case Ok:
        var t17 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv15 = t17
        return retv15
    case Err:
        var x2 string = mtmp0.(Err)._0
        var try_residual__12 string = x2
        var t18 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv15 = t18
        return retv15
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv20 string
    var jp22 string
    switch res__2.(type) {
    case Ok:
        var jp24 string
        jp24 = "ok unit"
        jp22 = jp24
    case Err:
        var x5 string = res__2.(Err)._0
        var err__3 string = x5
        var t25 string = "err " + err__3
        jp22 = t25
    default:
        panic("non-exhaustive match")
    }
    retv20 = jp22
    return retv20
}

func main0() struct{} {
    var t27 Result__unit__string = run(true)
    var t28 string = show(t27)
    println__T_string(t28)
    var t29 Result__unit__string = run(false)
    var t30 string = show(t29)
    println__T_string(t30)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
