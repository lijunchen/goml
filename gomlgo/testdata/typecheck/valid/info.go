package fixture

type Box[T any] struct {
	Value T
}

func (box Box[T]) Get() T {
	return box.Value
}

var Seed int8 = 1
var Result = Box[int]{Value: int(Seed)}.Get()

func compute(flag bool) int {
	value := Result
	if flag {
		value = 2
	}
	return value
}
