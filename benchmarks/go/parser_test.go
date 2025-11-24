package reflection

import (
	"encoding/json"
	"fmt"
	"testing"
)

var book = Book{Name: "John", Page_count: 124, Rating: 1, Release_year: 2016}
var bookS string = "{\"name\":\"Jonh \",\"page_count\":124,\"rating\":1,\"release_year\":2016}"

func BenchmarkSerialisingBookUnmarshal(b *testing.B) {
	b.ResetTimer()
	for range b.N {
		json.Marshal(book)
	}
}

func BenchmarkSerialisingBookRefl(b *testing.B) {
	b.ResetTimer()
	for range b.N {
		SerialiseBook(book)
	}
}

func mkBooks(size int) []Book {
	l := make([]Book, size)
	for i := range size {
		l[i] = book
	}
	return l
}

func SerialisingBooksRefl(b *testing.B, size int) {
	l := mkBooks(size)
	b.ResetTimer()
	for range b.N {
		SerialiseBooks(l)
	}
}

func SerialisingBooksUnmarshal(b *testing.B, size int) {
	l := mkBooks(size)
	b.ResetTimer()
	for range b.N {
		json.Marshal(l)
	}
}

func BenchmarkSerialisingBooksUnmarshal_10(b *testing.B)  { SerialisingBooksUnmarshal(b, 10) }
func BenchmarkSerialisingBooksRefl_10(b *testing.B)       { SerialisingBooksRefl(b, 10) }
func BenchmarkSerialisingBooksUnmarshal_50(b *testing.B)  { SerialisingBooksUnmarshal(b, 50) }
func BenchmarkSerialisingBooksRefl_50(b *testing.B)       { SerialisingBooksRefl(b, 50) }
func BenchmarkSerialisingBooksUnmarshal_100(b *testing.B) { SerialisingBooksUnmarshal(b, 100) }
func BenchmarkSerialisingBooksRefl_100(b *testing.B)      { SerialisingBooksRefl(b, 100) }

func BenchmarkParsingBookUnmarshal(b *testing.B) {
	s := []byte(bookS)
	bookO := Book{}
	b.ResetTimer()
	for range b.N {
		json.Unmarshal(s, &bookO)
	}
}

func BenchmarkParsingBookRefl(b *testing.B) {
	b.ResetTimer()
	for range b.N {
		ParseBook(bookS)
	}
}

func mkBooksS(size int) string {
	payload := "["
	for i := range size {
		if i < size-1 {
			payload = fmt.Sprintf("%s%s,", payload, bookS)
		} else {
			payload = fmt.Sprintf("%s%s", payload, bookS)
		}
	}

	payload = fmt.Sprintf("%s]", payload)
	return payload
}

func ParsingBooksRefl(b *testing.B, size int) {
	s := mkBooksS(size)
	b.ResetTimer()
	for range b.N {
		ParseBooks(s)
	}
}

func ParsingBooksUnmarshal(b *testing.B, size int) {
	books := []Book{}
	s := []byte(mkBooksS(size))
	b.ResetTimer()
	for range b.N {
		json.Unmarshal(s, &books)
	}
}

func BenchmarkParsingBooksUnmarshal_10(b *testing.B) { ParsingBooksUnmarshal(b, 10) }
func BenchmarkParsingBooksRefl_10(b *testing.B)      { ParsingBooksRefl(b, 10) }
func BenchmarkParsingBooksUnmarshal_50(b *testing.B) { ParsingBooksUnmarshal(b, 50) }
func BenchmarkParsingBooksRefl_50(b *testing.B)      { ParsingBooksRefl(b, 50) }
func BenchmarkParsingBooksUnmarshal_20(b *testing.B) { ParsingBooksUnmarshal(b, 20) }
func BenchmarkParsingBooksRefl_20(b *testing.B)      { ParsingBooksRefl(b, 20) }
