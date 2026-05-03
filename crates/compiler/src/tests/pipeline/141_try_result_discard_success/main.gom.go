package main

import (
    _goml_fmt "fmt"
)

func string_println(s string) struct{} {
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
    var retv9 Result__string__string
    var jp11 Result__string__string
    if ok__0 {
        var t12 Result__string__string = Ok{
            _0: "ignored",
        }
        jp11 = t12
    } else {
        var t13 Result__string__string = Err{
            _0: "parse failed",
        }
        jp11 = t13
    }
    retv9 = jp11
    return retv9
}

func check(ok__1 bool) Result__string__string {
    var retv15 Result__string__string
    var mtmp0 Result__string__string = parse_text(ok__1)
    switch mtmp0.(type) {
    case Ok:
        var t18 Result__string__string = Ok{
            _0: "ok",
        }
        retv15 = t18
        return retv15
    case Err:
        var x2 string = mtmp0.(Err)._0
        var try_residual__12 string = x2
        var t19 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv15 = t19
        return retv15
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv21 string
    var jp23 string
    switch res__2.(type) {
    case Ok:
        var x4 string = res__2.(Ok)._0
        var value__3 string = x4
        var t24 string = "ok " + value__3
        jp23 = t24
    case Err:
        var x5 string = res__2.(Err)._0
        var err__4 string = x5
        var t25 string = "err " + err__4
        jp23 = t25
    default:
        panic("non-exhaustive match")
    }
    retv21 = jp23
    return retv21
}

func main0() struct{} {
    var t27 Result__string__string = check(true)
    var t28 string = show(t27)
    println__T_string(t28)
    var t29 Result__string__string = check(false)
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
