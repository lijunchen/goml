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
    var inline507 uint = left__3 + right__4
    var inline508 uint = inline507 * 2
    result__5 = inline508
    var inline504 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    _goml_runtime_core_string_println(inline504)
    var t428 string
    var inline502 string = _goml_runtime_core_uint_to_string(result__5)
    t428 = inline502
    var inline499 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline499)
    var t429 string
    switch result__5 {
    case 0:
        t429 = "zero"
    case 42:
        t429 = "answer"
    default:
        t429 = "other"
    }
    var inline495 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline495)
    var t430 bool = result__5 > left__3
    var inline492 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t430)
    _goml_runtime_core_string_println(inline492)
    var t431_rhs uint = 15
    var t431 uint = result__5 & t431_rhs
    var t432 uint64 = uint64(uint(t431))
    var inline489 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(t432)
    _goml_runtime_core_string_println(inline489)
    var values__6 *hashmap_uint_string_x
    var inline487 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    values__6 = inline487
    var inline484 string = "stored"
    hashmap_set__HashMap_4uint_6string(values__6, result__5, inline484)
    var mtmp417 Option__string
    var inline481 uint = 42
    var inline482 Option__string = hashmap_get__HashMap_4uint_6string(values__6, inline481)
    mtmp417 = inline482
    switch mtmp417._tag {
    case 0:
        var inline474 string = "missing"
        var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline474)
        _goml_runtime_core_string_println(inline475)
        return struct{}{}
    case 1:
        var x418 string = mtmp417._v1_0
        var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x418)
        _goml_runtime_core_string_println(inline478)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__451 uint) string {
    var t444 string = _goml_runtime_core_uint_to_string(self__451)
    return t444
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t463 string = _goml_runtime_core_bool_to_string(self__148)
    return t463
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__159 uint64) string {
    var t466 string = _goml_runtime_core_uint64_to_string(self__159)
    return t466
}

func main() {
    main0()
}
