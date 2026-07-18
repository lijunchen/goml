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
    Loop_loop67:
    for {
        var t68 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(running__0)
        if t68 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop75:
                for {
                    var t76 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__2)
                    if t76 {
                        continue
                    } else {
                        break Loop_loop75
                    }
                }
                var scanning__3 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop72:
                for {
                    var t73 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__3)
                    if t73 {
                        continue
                    } else {
                        break Loop_loop72
                    }
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__0, false)
                continue
            case Close:
                var scanning__3 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop72__2:
                for {
                    var t73 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__3)
                    if t73 {
                        continue
                    } else {
                        break Loop_loop72__2
                    }
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__0, false)
                continue
            case Advance:
                var scanning__3 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop72__3:
                for {
                    var t73 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__3)
                    if t73 {
                        continue
                    } else {
                        break Loop_loop72__3
                    }
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__0, false)
                continue
            case Error:
                var scanning__3 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
                Loop_loop72__4:
                for {
                    var t73 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(scanning__3)
                    if t73 {
                        continue
                    } else {
                        break Loop_loop72__4
                    }
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(running__0, false)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop67
        }
    }
    _goml_runtime_core_string_println("ok")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv78 *ref_bool_x
    var t79 *ref_bool_x = ref__Ref_4bool(value__204)
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv81 bool
    var t82 bool = ref_get__Ref_4bool(self__205)
    retv81 = t82
    return retv81
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__206 *ref_bool_x, value__207 bool) struct{} {
    ref_set__Ref_4bool(self__206, value__207)
    return struct{}{}
}

func main() {
    main0()
}
