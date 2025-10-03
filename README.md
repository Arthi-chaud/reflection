# Reflection

This repository includes 3 Haskell libraries

- `reflection-base`, which exposes `observeType`, the `EDSL` typeclass and the `Type` ADT
- `reflection-serialiser`, a library that uses `observeType` to generate templates at compile time used to serialise data. It works with JSON and XML, but the API allows for more, user-defined formats.
- `reflection-json-parser`, a library that uses `oberseType`, `flatparse` and Template Haskell to generate type-specialised parsing function for JSON

## Benchmark suites

- `reflection-serialiser`
    - `micro`: Micro benchmarks, comparing the library's performance against `Aeson` and `xml` on lists, simple, non-recursive and recursive data types
    - `pandoc`: Comparing the time it takes for the `pandoc` library (not the executable) to output the JSON AST as a byte string of a medium-sized document using `Aeson` vs. the library.
    - `servant`: Comparing the round-trip-time of HTTP requests where the server returns a JSON-encoded tree using `Aeson` vs. the library
- `reflection-json-parser`
    - `micro`: Micro benchmarks, comparing the library's performance against `Aeson` on lists, simple, non-recursive and recursive data types.
- `reflection-json-parser-benchmark-server`: Compute the RTT of requests where the server parses JSON data using `Aeson` vs. the `reflection-json-parser` library. (Note: it has a variant where the server is written in Rust and uses `scanf` to parse the JSON input). 

To run the benchmarks, run:

```bash
stack bench $NAME
```

For example

```bash
stack bench reflection-serialiser:micro
stack bench reflection-serialiser:pandoc
stack bench reflection-json-parser:micro
```
