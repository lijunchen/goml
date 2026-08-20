package fixture

import "unsafe"

type small interface {
	~int8 | ~int16
}

func convert[P small](value int) P {
	return P(value)
}

func pointers(pointer *int) unsafe.Pointer {
	return unsafe.Pointer(pointer)
}

var bytes = []byte("go")
var array = [2]byte(bytes)
