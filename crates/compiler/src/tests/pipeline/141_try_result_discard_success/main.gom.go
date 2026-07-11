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
    var retv16 Result__string__string
    var jp18 Result__string__string
    if ok__0 {
        var t19 Result__string__string = Ok{
            _0: "ignored",
        }
        jp18 = t19
    } else {
        var t20 Result__string__string = Err{
            _0: "parse failed",
        }
        jp18 = t20
    }
    retv16 = jp18
    return retv16
}

func check(ok__1 bool) Result__string__string {
    var retv22 Result__string__string
    var mtmp7 Result__string__string = parse_text(ok__1)
    switch mtmp7.(type) {
    case Ok:
        var t25 Result__string__string = Ok{
            _0: "ok",
        }
        retv22 = t25
        return retv22
    case Err:
        var x9 string = mtmp7.(Err)._0
        var try_residual__12 string = x9
        var t26 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv22 = t26
        return retv22
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv28 string
    var jp30 string
    switch res__2.(type) {
    case Ok:
        var x11 string = res__2.(Ok)._0
        var value__3 string = x11
        var t31 string = "ok " + value__3
        jp30 = t31
    case Err:
        var x12 string = res__2.(Err)._0
        var err__4 string = x12
        var t32 string = "err " + err__4
        jp30 = t32
    default:
        panic("non-exhaustive match")
    }
    retv28 = jp30
    return retv28
}

func main0() struct{} {
    var t34 Result__string__string = check(true)
    var t35 string = show(t34)
    println__T_string(t35)
    var t36 Result__string__string = check(false)
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
