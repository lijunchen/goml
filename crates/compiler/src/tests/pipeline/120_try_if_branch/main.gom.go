package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

type GoError = error

func parse(flag__0 bool) Result__int32__string {
    var retv9 Result__int32__string
    var jp11 Result__int32__string
    if flag__0 {
        var t12 Result__int32__string = Ok{
            _0: 5,
        }
        jp11 = t12
    } else {
        var t13 Result__int32__string = Err{
            _0: "bad-branch",
        }
        jp11 = t13
    }
    retv9 = jp11
    return retv9
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var retv15 Result__int32__string
    var jp17 int32
    if flag__1 {
        var mtmp0 Result__int32__string = parse(fallback__2)
        var jp21 int32
        switch mtmp0.(type) {
        case Ok:
            var x1 int32 = mtmp0.(Ok)._0
            var try_value__13 int32 = x1
            jp21 = try_value__13
            jp17 = jp21
            var value__3 int32 = jp17
            var t18 int32 = value__3 + 1
            var t19 Result__int32__string = Ok{
                _0: t18,
            }
            retv15 = t19
            return retv15
        case Err:
            var x2 string = mtmp0.(Err)._0
            var try_residual__13 string = x2
            var t22 Result__int32__string = Err{
                _0: try_residual__13,
            }
            retv15 = t22
            return retv15
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp17 = 10
        var value__3 int32 = jp17
        var t18 int32 = value__3 + 1
        var t19 Result__int32__string = Ok{
            _0: t18,
        }
        retv15 = t19
        return retv15
    }
}

func show(res__4 Result__int32__string) string {
    var retv24 string
    var jp26 string
    switch res__4.(type) {
    case Ok:
        var x3 int32 = res__4.(Ok)._0
        var value__5 int32 = x3
        var t27 string = int32_to_string(value__5)
        var t28 string = "ok=" + t27
        jp26 = t28
    case Err:
        var x4 string = res__4.(Err)._0
        var err__6 string = x4
        var t29 string = "err=" + err__6
        jp26 = t29
    default:
        panic("non-exhaustive match")
    }
    retv24 = jp26
    return retv24
}

func main0() struct{} {
    var t31 Result__int32__string = bump(true, true)
    var t32 string = show(t31)
    println__T_string(t32)
    var t33 Result__int32__string = bump(true, false)
    var t34 string = show(t33)
    println__T_string(t34)
    var t35 Result__int32__string = bump(false, false)
    var t36 string = show(t35)
    println__T_string(t36)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
