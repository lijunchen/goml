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
    var retv17 Result__Handle__string
    var jp19 Result__Handle__string
    if ok__0 {
        var t20 Handle = Handle{
            name: "config",
        }
        var t21 Result__Handle__string = Result__Handle__string_Ok{
            _0: t20,
        }
        jp19 = t21
    } else {
        var t22 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp19 = t22
    }
    retv17 = jp19
    return retv17
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv24 Result__unit__string
    var jp26 Result__unit__string
    if ok__2 {
        var t27 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp26 = t27
    } else {
        var t28 string = handle__1.name
        var t29 string = "close failed for " + t28
        var t30 Result__unit__string = Result__unit__string_Err{
            _0: t29,
        }
        jp26 = t30
    }
    retv24 = jp26
    return retv24
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv32 Result__string__string
    var mtmp4 Result__Handle__string = open_handle(open_ok__3)
    var jp34 Handle
    switch mtmp4.(type) {
    case Result__Handle__string_Ok:
        var x5 Handle = mtmp4.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x5
        jp34 = try_value__24
        var handle__5 Handle = jp34
        var name__6 string = handle__5.name
        var mtmp7 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp7.(type) {
        case Result__unit__string_Ok:
            var t36 string = "closed " + name__6
            var t37 Result__string__string = Result__string__string_Ok{
                _0: t36,
            }
            retv32 = t37
            return retv32
        case Result__unit__string_Err:
            var x9 string = mtmp7.(Result__unit__string_Err)._0
            var try_residual__31 string = x9
            var t38 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv32 = t38
            return retv32
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x6 string = mtmp4.(Result__Handle__string_Err)._0
        var try_residual__24 string = x6
        var t39 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv32 = t39
        return retv32
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv41 string
    var jp43 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x11 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x11
        var t44 string = "ok " + value__8
        jp43 = t44
    case Result__string__string_Err:
        var x12 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x12
        var t45 string = "err " + err__9
        jp43 = t45
    default:
        panic("non-exhaustive match")
    }
    retv41 = jp43
    return retv41
}

func main0() struct{} {
    var t47 Result__string__string = use_handle(true, true)
    var t48 string = show(t47)
    println__T_string(t48)
    var t49 Result__string__string = use_handle(false, true)
    var t50 string = show(t49)
    println__T_string(t50)
    var t51 Result__string__string = use_handle(true, false)
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
