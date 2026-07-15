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
    var retv33 Result__string__string
    var jp35 Result__string__string
    if ok__0 {
        var t36 Result__string__string = Ok{
            _0: "goml",
        }
        jp35 = t36
    } else {
        var t37 Result__string__string = Err{
            _0: "parse failed",
        }
        jp35 = t37
    }
    retv33 = jp35
    return retv33
}

func normalize_text(ok__1 bool) Result__string__string {
    var retv39 Result__string__string
    var mtmp22 Result__string__string = parse_text(ok__1)
    var jp41 string
    switch mtmp22.(type) {
    case Ok:
        var x23 string = mtmp22.(Ok)._0
        var try_value__12 string = x23
        jp41 = try_value__12
        var text__2 string = jp41
        var t42 string = text__2 + "!"
        var t43 Result__string__string = Ok{
            _0: t42,
        }
        retv39 = t43
        return retv39
    case Err:
        var x24 string = mtmp22.(Err)._0
        var try_residual__12 string = x24
        var t44 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv39 = t44
        return retv39
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var retv46 Result__string__string
    var mtmp25 Result__string__string = normalize_text(ok__3)
    var jp48 string
    switch mtmp25.(type) {
    case Ok:
        var x26 string = mtmp25.(Ok)._0
        var try_value__20 string = x26
        jp48 = try_value__20
        var text__4 string = jp48
        var t49 string = "[" + text__4
        var t50 string = t49 + "]"
        var t51 Result__string__string = Ok{
            _0: t50,
        }
        retv46 = t51
        return retv46
    case Err:
        var x27 string = mtmp25.(Err)._0
        var try_residual__20 string = x27
        var t52 Result__string__string = Err{
            _0: try_residual__20,
        }
        retv46 = t52
        return retv46
    default:
        panic("non-exhaustive match")
    }
}

func show(res__5 Result__string__string) string {
    var retv54 string
    var jp56 string
    switch res__5.(type) {
    case Ok:
        var x28 string = res__5.(Ok)._0
        var value__6 string = x28
        var t57 string = "ok " + value__6
        jp56 = t57
    case Err:
        var x29 string = res__5.(Err)._0
        var err__7 string = x29
        var t58 string = "err " + err__7
        jp56 = t58
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func main0() struct{} {
    var t60 Result__string__string = decorate_text(true)
    var t61 string = show(t60)
    println__T_string(t61)
    var t62 Result__string__string = decorate_text(false)
    var t63 string = show(t62)
    println__T_string(t63)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t65 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t65)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv68 string
    retv68 = self__9
    return retv68
}

func main() {
    main0()
}
