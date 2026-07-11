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
    var retv31 Result__unit__string
    var jp33 Result__unit__string
    if ok__0 {
        var t34 Result__unit__string = Ok{
            _0: struct{}{},
        }
        jp33 = t34
    } else {
        var t35 Result__unit__string = Err{
            _0: "step failed",
        }
        jp33 = t35
    }
    retv31 = jp33
    return retv31
}

func run(ok__1 bool) Result__unit__string {
    var retv37 Result__unit__string
    var mtmp22 Result__unit__string = step(ok__1)
    switch mtmp22.(type) {
    case Ok:
        var t39 Result__unit__string = Ok{
            _0: struct{}{},
        }
        retv37 = t39
        return retv37
    case Err:
        var x24 string = mtmp22.(Err)._0
        var try_residual__12 string = x24
        var t40 Result__unit__string = Err{
            _0: try_residual__12,
        }
        retv37 = t40
        return retv37
    default:
        panic("non-exhaustive match")
    }
}

func show(res__2 Result__unit__string) string {
    var retv42 string
    var jp44 string
    switch res__2.(type) {
    case Ok:
        var jp46 string
        jp46 = "ok unit"
        jp44 = jp46
    case Err:
        var x27 string = res__2.(Err)._0
        var err__3 string = x27
        var t47 string = "err " + err__3
        jp44 = t47
    default:
        panic("non-exhaustive match")
    }
    retv42 = jp44
    return retv42
}

func main0() struct{} {
    var t49 Result__unit__string = run(true)
    var t50 string = show(t49)
    println__T_string(t50)
    var t51 Result__unit__string = run(false)
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
