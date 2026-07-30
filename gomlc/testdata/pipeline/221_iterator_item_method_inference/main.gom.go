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

type Values struct {}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_m_trait__impl_i_Iterator_i_Values_i_next(self__0 Values) Option__int32 {
    var retv114 Option__int32
    retv114 = None{}
    return retv114
}

func main0() struct{} {
    var t116 Values = Values{}
    var for_iter108 Values = _goml_m_trait__impl_i_IntoIterator_i_Values_i_into__iter(t116)
    Loop_loop118:
    for {
        if true {
            var for_next109 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Values_i_next(for_iter108)
            switch for_next109.(type) {
            case None:
                break Loop_loop118
            case Some:
                var x110 int32 = for_next109.(Some)._0
                var value__1 int32 = x110
                var t120 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
                _goml_runtime_core_string_println(t120)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop118
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Values_i_into__iter(self__109 Values) Values {
    var retv122 Values
    retv122 = self__109
    return retv122
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv124 string
    var t125 string = _goml_runtime_core_int32_to_string(self__6)
    retv124 = t125
    return retv124
}

func main() {
    main0()
}
