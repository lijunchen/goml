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
    var retv31 Result__string__string
    var jp33 Result__string__string
    if ok__0 {
        var t34 Result__string__string = Ok{
            _0: "ignored",
        }
        jp33 = t34
    } else {
        var t35 Result__string__string = Err{
            _0: "parse failed",
        }
        jp33 = t35
    }
    retv31 = jp33
    return retv31
}

func check(ok__1 bool) Result__string__string {
    var retv37 Result__string__string
    var mtmp22 Result__string__string = parse_text(ok__1)
    switch mtmp22.(type) {
    case Ok:
        var t40 Result__string__string = Ok{
            _0: "ok",
        }
        retv37 = t40
        return retv37
    case Err:
        var x24 string = mtmp22.(Err)._0
        var try_residual__12 string = x24
        var t41 Result__string__string = Err{
            _0: try_residual__12,
        }
        retv37 = t41
        return retv37
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__string__string) string {
    var retv43 string
    var jp45 string
    switch res__2.(type) {
    case Ok:
        var x26 string = res__2.(Ok)._0
        var value__3 string = x26
        var t46 string = "ok " + value__3
        jp45 = t46
    case Err:
        var x27 string = res__2.(Err)._0
        var err__4 string = x27
        var t47 string = "err " + err__4
        jp45 = t47
    default:
        panic("non-exhaustive match")
    }
    retv43 = jp45
    return retv43
}

func main0() struct{} {
    var t49 Result__string__string = check(true)
    var t50 string = show(t49)
    println__T_string(t50)
    var t51 Result__string__string = check(false)
    var t52 string = show(t51)
    println__T_string(t52)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t54 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t54)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv57 string
    retv57 = self__9
    return retv57
}

func main() {
    main0()
}
