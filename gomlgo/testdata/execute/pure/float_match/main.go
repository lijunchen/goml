package main

func classify(value float64) string {
	switch value {
	case 0.0:
		return "zero"
	case 1.0:
		return "one"
	case -1.0:
		return "minus one"
	case 3.14:
		return "pi"
	default:
		return "other"
	}
}

func main() {
	println(classify(0.0))
	println(classify(1.0))
	println(classify(-1.0))
	println(classify(3.14))
	println(classify(42.0))
	println(18318654708.7 < 18318654709.0)
	println(12345678.0 < 12345679.0)
}
