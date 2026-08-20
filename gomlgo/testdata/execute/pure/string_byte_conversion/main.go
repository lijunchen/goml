package main

func main() {
	value := "GoML 世界"
	bytes := []byte(value)
	bytes[0] = 'g'
	runes := []rune(value)
	runes[0] = 'g'
	invalidRunes := []rune(string([]byte{255}))
	println(string(bytes), value, len(bytes), string('世'), string(runes), string(-1), len(invalidRunes), invalidRunes[0])
}
