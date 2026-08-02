package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Counter struct {
    start uint32
    end uint64
}

func is_flag8(value__0 uint8) bool {
    var retv159 bool
    var jp161 bool
    switch value__0 {
    case 0:
        jp161 = true
    case 200:
        jp161 = true
    default:
        jp161 = false
    }
    retv159 = jp161
    return retv159
}

func is_flag16(value__1 uint16) bool {
    var retv163 bool
    var jp165 bool
    switch value__1 {
    case 1024:
        jp165 = true
    case 65000:
        jp165 = true
    default:
        jp165 = false
    }
    retv163 = jp165
    return retv163
}

func is_flag32(value__2 uint32) bool {
    var retv167 bool
    var jp169 bool
    switch value__2 {
    case 4000000000:
        jp169 = true
    case 1234567890:
        jp169 = true
    default:
        jp169 = false
    }
    retv167 = jp169
    return retv167
}

func is_flag64(value__3 uint64) bool {
    var retv171 bool
    var jp173 bool
    switch value__3 {
    case 900000000:
        jp173 = true
    case 600000000:
        jp173 = true
    default:
        jp173 = false
    }
    retv171 = jp173
    return retv171
}

func match_struct(counter__4 Counter) bool {
    var retv175 bool
    var x155 uint32 = counter__4.start
    var x156 uint64 = counter__4.end
    var jp177 bool
    switch x156 {
    case 900000000:
        var jp179 bool
        switch x155 {
        case 4000000000:
            jp179 = true
        default:
            jp179 = false
        }
        jp177 = jp179
    case 600000000:
        jp177 = true
    default:
        jp177 = false
    }
    retv175 = jp177
    return retv175
}

func report(label__5 string, value__6 bool) string {
    var retv181 string
    var t182 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t183 string = label__5 + t182
    retv181 = t183
    return retv181
}

func main0() struct{} {
    var counter__7 Counter = Counter{
        start: 4000000000,
        end: 900000000,
    }
    var alt_counter__8 Counter = Counter{
        start: 12,
        end: 600000000,
    }
    var t185 bool = is_flag8(200)
    var t186 string = report("u8_hit=", t185)
    var t187 bool = is_flag8(15)
    var t188 string = report(",u8_miss=", t187)
    var t189 string = t186 + t188
    var t190 bool = is_flag16(65000)
    var t191 string = report(",u16_hit=", t190)
    var t192 string = t189 + t191
    var t193 bool = is_flag16(42)
    var t194 string = report(",u16_miss=", t193)
    var t195 string = t192 + t194
    var t196 bool = is_flag32(1234567890)
    var t197 string = report(",u32_hit=", t196)
    var t198 string = t195 + t197
    var t199 bool = is_flag32(99)
    var t200 string = report(",u32_miss=", t199)
    var t201 string = t198 + t200
    var t202 bool = is_flag64(900000000)
    var t203 string = report(",u64_hit=", t202)
    var t204 string = t201 + t203
    var t205 bool = is_flag64(700000000)
    var t206 string = report(",u64_miss=", t205)
    var t207 string = t204 + t206
    var t208 bool = match_struct(counter__7)
    var t209 string = report(",struct_first=", t208)
    var t210 string = t207 + t209
    var t211 bool = match_struct(alt_counter__8)
    var t212 string = report(",struct_second=", t211)
    var message__9 string = t210 + t212
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv214 string
    var t215 string = _goml_runtime_core_bool_to_string(self__37)
    retv214 = t215
    return retv214
}

func println__T_string(value__1 string) struct{} {
    var t217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv220 string
    retv220 = self__38
    return retv220
}

func main() {
    main0()
}
