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
    var retv112 List__int32
    var t113 List__int32 = make__0(value__1, values__2)
    retv112 = t113
    return retv112
}

func sum(values__3 List__int32) int32 {
    var retv115 int32
    var jp117 int32
    switch values__3.(type) {
    case Nil:
        jp117 = 0
    case Cons:
        var x108 int32 = values__3.(Cons)._0
        var x109 List__int32 = values__3.(Cons)._1
        var rest__5 List__int32 = x109
        var value__4 int32 = x108
        var t118 int32 = sum(rest__5)
        var t119 int32 = value__4 + t118
        jp117 = t119
    default:
        panic("non-exhaustive match")
    }
    retv115 = jp117
    return retv115
}

func main0() struct{} {
    var cons__6 closure_env_cons_0 = closure_env_cons_0{}
    var t121 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 2, Nil{})
    var values__7 List__int32 = prepend_with(func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(cons__6, p0, p1)
    }, 1, t121)
    var t122 int32 = sum(values__7)
    var t123 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t122)
    println__T_string(t123)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv129 string
    var t130 string = _goml_runtime_core_int32_to_string(self__6)
    retv129 = t130
    return retv129
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv132 string
    retv132 = self__38
    return retv132
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env110 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var retv134 List__int32
    var t135 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    retv134 = t135
    return retv134
}

func main() {
    main0()
}
