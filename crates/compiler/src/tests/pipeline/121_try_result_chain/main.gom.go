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
    var retv15 Result__string__string
    var jp17 Result__string__string
    if ok__0 {
        var t18 Result__string__string = Ok{
            _0: "goml",
        }
        jp17 = t18
    } else {
        var t19 Result__string__string = Err{
            _0: "parse failed",
        }
        jp17 = t19
    }
    retv15 = jp17
    return retv15
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv21 Result__string__string
    var mtmp4 Result__string__string = parse_text(ok__1)
    var jp23 string
    switch mtmp4.(type) {
    case Ok:
        var x5 string = mtmp4.(Ok)._0
        var try_value__12 string = x5
        jp23 = try_value__12
        var text__2 string = jp23
        var t24 string = text__2 + "!"
        var t25 Result__string__string = Ok{
            _0: t24,
        }
        retv21 = t25
        return retv21
    case Err:
        var x6 string = mtmp4.(Err)._0
        var try_residual__12 string = x6
        var t26 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv21 = t26
        return retv21
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv28 Result__string__string
    var mtmp7 Result__string__string = normalize_text(ok__3)
    var jp30 string
    switch mtmp7.(type) {
    case Ok:
        var x8 string = mtmp7.(Ok)._0
        var try_value__20 string = x8
        jp30 = try_value__20
        var text__4 string = jp30
        var t31 string = "[" + text__4
        var t32 string = t31 + "]"
        var t33 Result__string__string = Ok{
            _0: t32,
        }
        retv28 = t33
        return retv28
    case Err:
        var x9 string = mtmp7.(Err)._0
        var try_residual__20 string = x9
        var t34 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv28 = t34
        return retv28
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv36 string
    var jp38 string
    switch res__5.(type) {
    case Ok:
        var x10 string = res__5.(Ok)._0
        var value__6 string = x10
        var t39 string = "ok " + value__6
        jp38 = t39
    case Err:
        var x11 string = res__5.(Err)._0
        var err__7 string = x11
        var t40 string = "err " + err__7
        jp38 = t40
    default:
        panic("non-exhaustive match")
    }
    retv36 = jp38
    return retv36
}

func main0() struct{} {
    var t42 Result__string__string = decorate_text(true)
    var t43 string = show(t42)
    println__T_string(t43)
    var t44 Result__string__string = decorate_text(false)
    var t45 string = show(t44)
    println__T_string(t45)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t47 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t47)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv50 string
    retv50 = self__9
    return retv50
}

func main() {
    main0()
}
