package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Handle struct {
    name string
}

type Result__Handle__string interface {
    isResult__Handle__string()
}

type Result__Handle__string_Ok struct {
    _0 Handle
}

func (_ Result__Handle__string_Ok) isResult__Handle__string() {}

type Result__Handle__string_Err struct {
    _0 string
}

func (_ Result__Handle__string_Err) isResult__Handle__string() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func open_handle(ok__0 bool) Result__Handle__string {
    var retv35 Result__Handle__string
    var jp37 Result__Handle__string
    if ok__0 {
        var t38 Handle = Handle{
            name: "config",
        }
        var t39 Result__Handle__string = Result__Handle__string_Ok{
            _0: t38,
        }
        jp37 = t39
    } else {
        var t40 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp37 = t40
    }
    retv35 = jp37
    return retv35
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv42 Result__unit__string
    var jp44 Result__unit__string
    if ok__2 {
        var t45 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp44 = t45
    } else {
        var t46 string = handle__1.name
        var t47 string = "close failed for " + t46
        var t48 Result__unit__string = Result__unit__string_Err{
            _0: t47,
        }
        jp44 = t48
    }
    retv42 = jp44
    return retv42
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv50 Result__string__string
    var mtmp22 Result__Handle__string = open_handle(open_ok__3)
    var jp52 Handle
    switch mtmp22.(type) {
    case Result__Handle__string_Ok:
        var x23 Handle = mtmp22.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x23
        jp52 = try_value__24
        var handle__5 Handle = jp52
        var name__6 string = handle__5.name
        var mtmp25 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp25.(type) {
        case Result__unit__string_Ok:
            var t54 string = "closed " + name__6
            var t55 Result__string__string = Result__string__string_Ok{
                _0: t54,
            }
            retv50 = t55
            return retv50
        case Result__unit__string_Err:
            var x27 string = mtmp25.(Result__unit__string_Err)._0
            var try_residual__31 string = x27
            var t56 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv50 = t56
            return retv50
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x24 string = mtmp22.(Result__Handle__string_Err)._0
        var try_residual__24 string = x24
        var t57 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv50 = t57
        return retv50
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv59 string
    var jp61 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x29 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x29
        var t62 string = "ok " + value__8
        jp61 = t62
    case Result__string__string_Err:
        var x30 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x30
        var t63 string = "err " + err__9
        jp61 = t63
    default:
        panic("non-exhaustive match")
    }
    retv59 = jp61
    return retv59
}

func main0() struct{} {
    var t65 Result__string__string = use_handle(true, true)
    var t66 string = show(t65)
    println__T_string(t66)
    var t67 Result__string__string = use_handle(false, true)
    var t68 string = show(t67)
    println__T_string(t68)
    var t69 Result__string__string = use_handle(true, false)
    var t70 string = show(t69)
    println__T_string(t70)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv75 string
    retv75 = self__9
    return retv75
}

func main() {
    main0()
}
