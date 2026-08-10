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

func sum(values__3 List__int32) int32 {
    switch values__3.(type) {
    case Nil:
        return 0
    case Cons:
        var x172 int32 = values__3.(Cons)._0
        var x173 List__int32 = values__3.(Cons)._1
        var t182 int32 = sum(x173)
        var t183 int32 = x172 + t182
        return t183
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t185 closure_env_cons_0 = closure_env_cons_0{}
    var cons__6 func(int32, List__int32) List__int32 = func(p0 int32, p1 List__int32) List__int32 {
        return _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(t185, p0, p1)
    }
    var t186 List__int32
    var inline210 int32 = 2
    var inline211 List__int32 = cons__6(inline210, Nil{})
    t186 = inline211
    var values__7 List__int32
    var inline207 int32 = 1
    var inline208 List__int32 = cons__6(inline207, t186)
    values__7 = inline208
    var t187 int32 = sum(values__7)
    var t188 string
    var inline205 string = _goml_runtime_core_int32_to_string(t187)
    t188 = inline205
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__cons__0_i_closure__env__cons__0_i_apply(env174 closure_env_cons_0, ctor_arg_0 int32, ctor_arg_1 List__int32) List__int32 {
    var t200 List__int32 = Cons{
        _0: ctor_arg_0,
        _1: ctor_arg_1,
    }
    return t200
}

func main() {
    main0()
}
