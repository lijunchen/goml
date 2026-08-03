package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_main_0 struct {}

type Maybe__int interface {
    isMaybe__int()
}

type Maybe__int_None struct {}

func (_ Maybe__int_None) isMaybe__int() {}

type Maybe__int_Some struct {
    _0 int
}

func (_ Maybe__int_Some) isMaybe__int() {}

type Maybe__string interface {
    isMaybe__string()
}

type Maybe__string_None struct {}

func (_ Maybe__string_None) isMaybe__string() {}

type Maybe__string_Some struct {
    _0 string
}

func (_ Maybe__string_Some) isMaybe__string() {}

func main0() struct{} {
    var value__3 Maybe__int = Maybe__int_Some{
        _0: 3,
    }
    var t182 closure_env_main_0 = closure_env_main_0{}
    var mtmp178 Maybe__string = _goml_m_inherent_i_Maybe_i_Maybe_l_T_r__i_map____T__int____U__string(value__3, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t182, p0)
    })
    switch mtmp178.(type) {
    case Maybe__string_None:
        var inline204 string = "none"
        var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline204)
        _goml_runtime_core_string_println(inline205)
        return struct{}{}
    case Maybe__string_Some:
        var x179 string = mtmp178.(Maybe__string_Some)._0
        var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x179)
        _goml_runtime_core_string_println(inline208)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Maybe_i_Maybe_l_T_r__i_map____T__int____U__string(self__0 Maybe__int, map_fn__1 func(int) string) Maybe__string {
    switch self__0.(type) {
    case Maybe__int_None:
        return Maybe__string_None{}
    case Maybe__int_Some:
        var x177 int = self__0.(Maybe__int_Some)._0
        var t193 string = map_fn__1(x177)
        var t194 Maybe__string = Maybe__string_Some{
            _0: t193,
        }
        return t194
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env180 closure_env_main_0, item__4 int) string {
    var inline212 string = _goml_runtime_core_int_to_string(item__4)
    return inline212
}

func main() {
    main0()
}
