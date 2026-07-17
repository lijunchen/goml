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
    var retv71 Result__Handle__string
    var jp73 Result__Handle__string
    if ok__0 {
        var t74 Handle = Handle{
            name: "config",
        }
        var t75 Result__Handle__string = Result__Handle__string_Ok{
            _0: t74,
        }
        jp73 = t75
    } else {
        var t76 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp73 = t76
    }
    retv71 = jp73
    return retv71
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv78 Result__unit__string
    var jp80 Result__unit__string
    if ok__2 {
        var t81 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp80 = t81
    } else {
        var t82 string = handle__1.name
        var t83 string = "close failed for " + t82
        var t84 Result__unit__string = Result__unit__string_Err{
            _0: t83,
        }
        jp80 = t84
    }
    retv78 = jp80
    return retv78
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv86 Result__string__string
    var mtmp58 Result__Handle__string = open_handle(open_ok__3)
    var jp88 Handle
    switch mtmp58.(type) {
    case Result__Handle__string_Ok:
        var x59 Handle = mtmp58.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x59
        jp88 = try_value__24
        var handle__5 Handle = jp88
        var name__6 string = handle__5.name
        var mtmp61 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp61.(type) {
        case Result__unit__string_Ok:
            var t90 string = "closed " + name__6
            var t91 Result__string__string = Result__string__string_Ok{
                _0: t90,
            }
            retv86 = t91
            return retv86
        case Result__unit__string_Err:
            var x63 string = mtmp61.(Result__unit__string_Err)._0
            var try_residual__31 string = x63
            var t92 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv86 = t92
            return retv86
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x60 string = mtmp58.(Result__Handle__string_Err)._0
        var try_residual__24 string = x60
        var t93 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv86 = t93
        return retv86
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv95 string
    var jp97 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x65 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x65
        var t98 string = "ok " + value__8
        jp97 = t98
    case Result__string__string_Err:
        var x66 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x66
        var t99 string = "err " + err__9
        jp97 = t99
    default:
        panic("non-exhaustive match")
    }
    retv95 = jp97
    return retv95
}

func main0() struct{} {
    var t101 Result__string__string = use_handle(true, true)
    var t102 string = show(t101)
    println__T_string(t102)
    var t103 Result__string__string = use_handle(false, true)
    var t104 string = show(t103)
    println__T_string(t104)
    var t105 Result__string__string = use_handle(true, false)
    var t106 string = show(t105)
    println__T_string(t106)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t108 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t108)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv111 string
    retv111 = self__34
    return retv111
}

func main() {
    main0()
}
