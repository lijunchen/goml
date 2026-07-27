package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_cons_0 struct {}

type List__int32 interface {
    isList__int32()
}

type Nil struct {}

func (_ Nil) isList__int32() {}

type Cons struct {
    _0 int32
    _1 List__int32
}

func (_ Cons) isList__int32() {}

func prepend_with(make__0 func(int32, List__int32) List__int32, value__1 int32, values__2 List__int32) List__int32 {
    var retv68 List__int32
    var t69 List__int32 = make__0(value__1, values__2)
    retv68 = t69
    return retv68
}

func sum(values__3 List__int32) int32 {
    var retv71 int32
    var jp73 int32
    switch values__3.(type) {
    case Nil:
        jp73 = 0
    case Cons:
        var x64 int32 = values__3.(Cons)._0
        var x65 List__int32 = values__3.(Cons)._1
        var rest__5 List__int32 = x65
        var value__4 int32 = x64
        var t74 int32 = sum(rest__5)
        var t75 int32 = value__4 + t74
        jp73 = t75
    default:
        panic("non-exhaustive match")
    }
    retv71 = jp73
    return retv71
}

func main0() struct{} {
    var cons__6 closure_env_cons_0 = closure_env_cons_0{}
    var t77 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 2, Nil{})
    var values__7 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 1, t77)
    var t78 int32 = sum(values__7)
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t78)
    println__T_string(t79)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t82 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t82)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv85 string
    var t86 string = _goml_runtime_core_int32_to_string(self__6)
    retv85 = t86
    return retv85
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv88 string
    retv88 = self__38
    return retv88
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env66 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var retv90 List__int32
    var t91 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    retv90 = t91
    return retv90
}

func main() {
    main0()
}
