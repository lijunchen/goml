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

func _goml_runtime_core_uint_to_string(x uint) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type hashmap_uint_string_x_entry struct {
    active bool
    key uint
    value string
}

type hashmap_uint_string_x struct {
    indices map[uint]int
    entries []hashmap_uint_string_x_entry
    len int
}

func hashmap_new__HashMap_4uint_6string() *hashmap_uint_string_x {
    return &hashmap_uint_string_x{
        indices: make(map[uint]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_lookup__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero string
        return zero, false
    }
    var entry hashmap_uint_string_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_4uint_6string(m, key)
    if ok {
        return Option__string{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__string{
        _tag: 0,
    }
}

func hashmap_set__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint, value string) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_uint_string_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_uint_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_uint_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
}

type Ordering int32

type Option__string struct {
    _tag int32
    _v1_0 string
}

func main0() struct{} {
    var left__3 uint = 19
    var right__4_source int = 2
    var right__4 uint = uint(int(right__4_source))
    var result__5 uint
    var inline504 uint = left__3 + right__4
    var inline505 uint = inline504 * 2
    result__5 = inline505
    var inline501 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    _goml_runtime_core_string_println(inline501)
    var t425 string
    var inline499 string = _goml_runtime_core_uint_to_string(result__5)
    t425 = inline499
    var inline496 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline496)
    var t426 string
    switch result__5 {
    case 0:
        t426 = "zero"
    case 42:
        t426 = "answer"
    default:
        t426 = "other"
    }
    var inline492 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline492)
    var t427 bool = result__5 > left__3
    var inline489 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t427)
    _goml_runtime_core_string_println(inline489)
    var t428_rhs uint = 15
    var t428 uint = result__5 & t428_rhs
    var t429 uint64 = uint64(uint(t428))
    var inline486 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(t429)
    _goml_runtime_core_string_println(inline486)
    var values__6 *hashmap_uint_string_x
    var inline484 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    values__6 = inline484
    var inline481 string = "stored"
    hashmap_set__HashMap_4uint_6string(values__6, result__5, inline481)
    var mtmp414 Option__string
    var inline478 uint = 42
    var inline479 Option__string = hashmap_get__HashMap_4uint_6string(values__6, inline478)
    mtmp414 = inline479
    switch mtmp414._tag {
    case 0:
        var inline471 string = "missing"
        var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline471)
        _goml_runtime_core_string_println(inline472)
        return struct{}{}
    case 1:
        var x415 string = mtmp414._v1_0
        var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x415)
        _goml_runtime_core_string_println(inline475)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__442 uint) string {
    var t441 string = _goml_runtime_core_uint_to_string(self__442)
    return t441
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t460 string = _goml_runtime_core_bool_to_string(self__148)
    return t460
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__159 uint64) string {
    var t463 string = _goml_runtime_core_uint64_to_string(self__159)
    return t463
}

func main() {
    main0()
}
