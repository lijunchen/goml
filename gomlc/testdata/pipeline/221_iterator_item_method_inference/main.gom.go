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
    var retv74 Option__int32
    retv74 = None{}
    return retv74
}

func main0() struct{} {
    var t76 Values = Values{}
    var for_iter68 Values = _goml_m_trait__impl_i_IntoIterator_i_Values_i_into__iter(t76)
    Loop_loop78:
    for {
        if true {
            var for_next69 Option__int32 = _goml_m_trait__impl_i_Iterator_i_Values_i_next(for_iter68)
            switch for_next69.(type) {
            case None:
                break Loop_loop78
            case Some:
                var x70 int32 = for_next69.(Some)._0
                var value__1 int32 = x70
                var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
                _goml_runtime_core_string_println(t80)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop78
        }
    }
    return struct{}{}
}

func _goml_m_trait__impl_i_IntoIterator_i_Values_i_into__iter(self__109 Values) Values {
    var retv82 Values
    retv82 = self__109
    return retv82
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__6)
    retv84 = t85
    return retv84
}

func main() {
    main0()
}
