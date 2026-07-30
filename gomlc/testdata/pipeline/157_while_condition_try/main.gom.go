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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

func step_some(i__0 int32) Option__bool {
    var retv122 Option__bool
    var t125 bool = i__0 < 3
    var jp124 Option__bool
    if t125 {
        var t126 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp124 = t126
    } else {
        var t127 Option__bool = Option__bool_Some{
            _0: false,
        }
        jp124 = t127
    }
    retv122 = jp124
    return retv122
}

func step_none(i__1 int32) Option__bool {
    var retv129 Option__bool
    var t132 bool = i__1 < 2
    var jp131 Option__bool
    if t132 {
        var t133 Option__bool = Option__bool_Some{
            _0: true,
        }
        jp131 = t133
    } else {
        jp131 = Option__bool_None{}
    }
    retv129 = jp131
    return retv129
}

func run_some() Option__int32 {
    var retv135 Option__int32
    var i__2 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop139:
    for {
        var t140 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
        var mtmp108 Option__bool = step_some(t140)
        var jp142 bool
        switch mtmp108.(type) {
        case Option__bool_None:
            retv135 = Option__int32_None{}
            return retv135
        case Option__bool_Some:
            var x109 bool = mtmp108.(Option__bool_Some)._0
            var try_value__31 bool = x109
            jp142 = try_value__31
            if jp142 {
                var t143 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
                var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t145 int32 = t143 + t144
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__3, t145)
                var t146 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__2)
                var t147 int32 = t146 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__2, t147)
                continue
            } else {
                break Loop_loop139
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t137 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__3)
    var t138 Option__int32 = Option__int32_Some{
        _0: t137,
    }
    retv135 = t138
    return retv135
}

func run_none() Option__int32 {
    var retv149 Option__int32
    var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var total__5 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop153:
    for {
        var t154 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
        var mtmp113 Option__bool = step_none(t154)
        var jp156 bool
        switch mtmp113.(type) {
        case Option__bool_None:
            retv149 = Option__int32_None{}
            return retv149
        case Option__bool_Some:
            var x114 bool = mtmp113.(Option__bool_Some)._0
            var try_value__67 bool = x114
            jp156 = try_value__67
            if jp156 {
                var t157 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
                var t158 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t159 int32 = t157 + t158
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(total__5, t159)
                var t160 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t161 int32 = t160 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t161)
                continue
            } else {
                break Loop_loop153
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t151 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(total__5)
    var t152 Option__int32 = Option__int32_Some{
        _0: t151,
    }
    retv149 = t152
    return retv149
}

func show(x__6 Option__int32) string {
    var retv163 string
    var jp165 string
    switch x__6.(type) {
    case Option__int32_None:
        jp165 = "none"
    case Option__int32_Some:
        var x118 int32 = x__6.(Option__int32_Some)._0
        var v__7 int32 = x118
        var t166 string = _goml_m_inherent_i_int32_i_int32_i_to__string(v__7)
        var t167 string = "some=" + t166
        jp165 = t167
    default:
        panic("non-exhaustive match")
    }
    retv163 = jp165
    return retv163
}

func main0() struct{} {
    var t169 Option__int32 = run_some()
    var t170 string = show(t169)
    println__T_string(t170)
    var t171 Option__int32 = run_none()
    var t172 string = show(t171)
    println__T_string(t172)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv174 *ref_int32_x
    var t175 *ref_int32_x = ref__Ref_5int32(value__207)
    retv174 = t175
    return retv174
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv177 int32
    var t178 int32 = ref_get__Ref_5int32(self__208)
    retv177 = t178
    return retv177
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv182 string
    var t183 string = _goml_runtime_core_int32_to_string(self__6)
    retv182 = t183
    return retv182
}

func println__T_string(value__1 string) struct{} {
    var t185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t185)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv188 string
    retv188 = self__38
    return retv188
}

func main() {
    main0()
}
