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
    var retv20 Result__Handle__string
    var jp22 Result__Handle__string
    if ok__0 {
        var t23 Handle = Handle{
            name: "config",
        }
        var t24 Result__Handle__string = Result__Handle__string_Ok{
            _0: t23,
        }
        jp22 = t24
    } else {
        var t25 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp22 = t25
    }
    retv20 = jp22
    return retv20
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv27 Result__unit__string
    var jp29 Result__unit__string
    if ok__2 {
        var t30 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp29 = t30
    } else {
        var t31 string = handle__1.name
        var t32 string = "close failed for " + t31
        var t33 Result__unit__string = Result__unit__string_Err{
            _0: t32,
        }
        jp29 = t33
    }
    retv27 = jp29
    return retv27
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv35 Result__string__string
    var mtmp7 Result__Handle__string = open_handle(open_ok__3)
    var jp37 Handle
    switch mtmp7.(type) {
    case Result__Handle__string_Ok:
        var x8 Handle = mtmp7.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x8
        jp37 = try_value__24
        var handle__5 Handle = jp37
        var name__6 string = handle__5.name
        var mtmp10 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp10.(type) {
        case Result__unit__string_Ok:
            var t39 string = "closed " + name__6
            var t40 Result__string__string = Result__string__string_Ok{
                _0: t39,
            }
            retv35 = t40
            return retv35
        case Result__unit__string_Err:
            var x12 string = mtmp10.(Result__unit__string_Err)._0
            var try_residual__31 string = x12
            var t41 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv35 = t41
            return retv35
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x9 string = mtmp7.(Result__Handle__string_Err)._0
        var try_residual__24 string = x9
        var t42 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv35 = t42
        return retv35
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv44 string
    var jp46 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x14 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x14
        var t47 string = "ok " + value__8
        jp46 = t47
    case Result__string__string_Err:
        var x15 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x15
        var t48 string = "err " + err__9
        jp46 = t48
    default:
        panic("non-exhaustive match")
    }
    retv44 = jp46
    return retv44
}

func main0() struct{} {
    var t50 Result__string__string = use_handle(true, true)
    var t51 string = show(t50)
    println__T_string(t51)
    var t52 Result__string__string = use_handle(false, true)
    var t53 string = show(t52)
    println__T_string(t53)
    var t54 Result__string__string = use_handle(true, false)
    var t55 string = show(t54)
    println__T_string(t55)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t57 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t57)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv60 string
    retv60 = self__9
    return retv60
}

func main() {
    main0()
}
