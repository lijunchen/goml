package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type Event int32

const (
    Open Event = 0
    Close Event = 1
    Advance Event = 2
    Error Event = 3
)

func main0() struct{} {
    var running__0 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
    Loop_loop158:
    for {
        var t159 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__0)
        if t159 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop166:
                for {
                    var t167 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__2)
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166
                    }
                }
                var scanning__3 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop163:
                for {
                    var t164 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__3)
                    if t164 {
                        continue
                    } else {
                        break Loop_loop163
                    }
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__0, false)
                continue
            case Close:
                var scanning__3 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop163__2:
                for {
                    var t164 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__3)
                    if t164 {
                        continue
                    } else {
                        break Loop_loop163__2
                    }
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__0, false)
                continue
            case Advance:
                var scanning__3 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop163__3:
                for {
                    var t164 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__3)
                    if t164 {
                        continue
                    } else {
                        break Loop_loop163__3
                    }
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__0, false)
                continue
            case Error:
                var scanning__3 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop163__4:
                for {
                    var t164 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__3)
                    if t164 {
                        continue
                    } else {
                        break Loop_loop163__4
                    }
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__0, false)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop158
        }
    }
    _goml_runtime_core_string_println("ok")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv169 *ref_bool_x
    var t170 *ref_bool_x = ref__Ref_4bool(value__207)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv172 bool
    var t173 bool = ref_get__Ref_4bool(self__208)
    retv172 = t173
    return retv172
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func main() {
    main0()
}
