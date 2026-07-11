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
    var retv18 Result__string__string
    var jp20 Result__string__string
    if ok__0 {
        var t21 Result__string__string = Ok{
            _0: "goml",
        }
        jp20 = t21
    } else {
        var t22 Result__string__string = Err{
            _0: "parse failed",
        }
        jp20 = t22
    }
    retv18 = jp20
    return retv18
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv24 Result__string__string
    var mtmp7 Result__string__string = parse_text(ok__1)
    var jp26 string
    switch mtmp7.(type) {
    case Ok:
        var x8 string = mtmp7.(Ok)._0
        var try_value__12 string = x8
        jp26 = try_value__12
        var text__2 string = jp26
        var t27 string = text__2 + "!"
        var t28 Result__string__string = Ok{
            _0: t27,
        }
        retv24 = t28
        return retv24
    case Err:
        var x9 string = mtmp7.(Err)._0
        var try_residual__12 string = x9
        var t29 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv24 = t29
        return retv24
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv31 Result__string__string
    var mtmp10 Result__string__string = normalize_text(ok__3)
    var jp33 string
    switch mtmp10.(type) {
    case Ok:
        var x11 string = mtmp10.(Ok)._0
        var try_value__20 string = x11
        jp33 = try_value__20
        var text__4 string = jp33
        var t34 string = "[" + text__4
        var t35 string = t34 + "]"
        var t36 Result__string__string = Ok{
            _0: t35,
        }
        retv31 = t36
        return retv31
    case Err:
        var x12 string = mtmp10.(Err)._0
        var try_residual__20 string = x12
        var t37 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv31 = t37
        return retv31
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv39 string
    var jp41 string
    switch res__5.(type) {
    case Ok:
        var x13 string = res__5.(Ok)._0
        var value__6 string = x13
        var t42 string = "ok " + value__6
        jp41 = t42
    case Err:
        var x14 string = res__5.(Err)._0
        var err__7 string = x14
        var t43 string = "err " + err__7
        jp41 = t43
    default:
        panic("non-exhaustive match")
    }
    retv39 = jp41
    return retv39
}

func main0() struct{} {
    var t45 Result__string__string = decorate_text(true)
    var t46 string = show(t45)
    println__T_string(t46)
    var t47 Result__string__string = decorate_text(false)
    var t48 string = show(t47)
    println__T_string(t48)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t50 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t50)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv53 string
    retv53 = self__9
    return retv53
}

func main() {
    main0()
}
