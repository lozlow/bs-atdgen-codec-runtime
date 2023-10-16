exception DecodeErrorPath(list<string>, string)
exception DecodeError = JsonCombinators.Json_Decode.DecodeError

type t<'a> = Js.Json.t => 'a

let make = f => f

let decode' = (f, json) => f(json)

let decode = (f, json) =>
  try f(json) catch {
  | DecodeErrorPath(path, msg) =>
    let path = String.concat(".", path)
    raise(DecodeError(`${path}: ${msg}`))
  }

let with_segment = (segment, f, json) =>
  try f(json) catch {
  | DecodeError(msg) => raise(DecodeErrorPath(list{segment}, msg))
  | DecodeErrorPath(path, msg) => raise(DecodeErrorPath(list{segment, ...path}, msg))
  }

let unit = j =>
  if Js.Json.test(j, Null) {
    ()
  } else {
    raise(DecodeError("Expected null, got " ++ Js.Json.stringify(j)))
  }

let decodeWith = (json, decoder) => {
  let decode_ = json => {
    switch json->JsonCombinators.Json_Decode.decode(decoder) {
    | Ok(json) => json
    | Error(message) => raise(DecodeError(message))
    }
  }

  decode(decode_, json)
}

let string = json => json->decodeWith(JsonCombinators.Json_Decode.string)

let char = json => {
  let string = string(json)
  if string->String.length === 1 {
    string->String.get(0)
  } else {
    raise(DecodeError(`Expected single-character string, got ${json->Js.Json.stringify}`))
  }
}

let float = json => json->decodeWith(JsonCombinators.Json_Decode.float)

let int = json => json->decodeWith(JsonCombinators.Json_Decode.int)

let bool = json => json->decodeWith(JsonCombinators.Json_Decode.bool)

let int32 = j => j->string->Int32.of_string

let int64 = j => j->string->Int64.of_string

let array = (decode, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic(json)
    let length = Js.Array.length(source)
    let target = Belt.Array.makeUninitializedUnsafe(length)
    for i in 0 to length - 1 {
      let value = try with_segment(string_of_int(i), decode, Array.unsafe_get(source, i)) catch {
      | DecodeError(msg) =>
        raise(DecodeError(msg ++ ("\n\tin array at index " ++ string_of_int(i))))
      }

      Array.unsafe_set(target, i, value)
    }
    target
  } else {
    raise(DecodeError("Expected array, got " ++ Js.Json.stringify(json)))
  }

let list = (decode, json) => json |> array(decode) |> Array.to_list

let pair = (decodeA, decodeB, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 2 {
      try (
        with_segment("0", decodeA, Array.unsafe_get(source, 0)),
        with_segment("1", decodeB, Array.unsafe_get(source, 1)),
      ) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin pair/tuple2"))
      }
    } else {
      raise(
        DecodeError(`Expected array of length 2, got array of length ${length->Belt.Int.toString}`),
      )
    }
  } else {
    raise(DecodeError("Expected array, got " ++ Js.Json.stringify(json)))
  }

let tuple2 = pair

let tuple3 = (decodeA, decodeB, decodeC, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 3 {
      try (
        with_segment("0", decodeA, Array.unsafe_get(source, 0)),
        with_segment("1", decodeB, Array.unsafe_get(source, 1)),
        with_segment("2", decodeC, Array.unsafe_get(source, 2)),
      ) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple3"))
      }
    } else {
      \"@@"(
        raise,
        DecodeError(`Expected array of length 3, got array of length ${length->Belt.Int.toString}`),
      )
    }
  } else {
    raise(DecodeError("Expected array, got " ++ Js.Json.stringify(json)))
  }

let tuple4 = (decodeA, decodeB, decodeC, decodeD, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 4 {
      try (
        with_segment("1", decodeA, Array.unsafe_get(source, 0)),
        with_segment("2", decodeB, Array.unsafe_get(source, 1)),
        with_segment("3", decodeC, Array.unsafe_get(source, 2)),
        with_segment("4", decodeD, Array.unsafe_get(source, 3)),
      ) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple4"))
      }
    } else {
      \"@@"(
        raise,
        DecodeError(`Expected array of length 4, got array of length ${length->Belt.Int.toString}`),
      )
    }
  } else {
    raise(DecodeError("Expected array, got " ++ Js.Json.stringify(json)))
  }

let dict = (decode, json) =>
  if Js.Json.test(json, Object) {
    let source: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let keys = Js.Dict.keys(source)
    let l = Js.Array.length(keys)
    let target = Js.Dict.empty()
    for i in 0 to l - 1 {
      let key = Array.unsafe_get(keys, i)
      let value = try with_segment(key, decode, Js.Dict.unsafeGet(source, key)) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin dict"))
      }

      Js.Dict.set(target, key, value)
    }
    target
  } else {
    raise(DecodeError("Expected object, got " ++ Js.Json.stringify(json)))
  }

let field = (key, decode, json) =>
  if Js.Json.test(json, Object) {
    let dict: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))
    switch Js.Dict.get(dict, key) {
    | Some(value) =>
      try with_segment(key, decode, value) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ ("\n\tat field '" ++ (key ++ "'"))))
      }
    | None => raise(DecodeError(`Expected field '${key}'`))
    }
  } else {
    raise(DecodeError("Expected object, got " ++ Js.Json.stringify(json)))
  }

let obj_array = (f, json) => dict(f, json) |> Js.Dict.entries

let obj_list = (f, json) => obj_array(f, json) |> Array.to_list

let nullable = (decode, json) =>
  if Js.Json.test(json, Null) {
    None
  } else {
    Some(decode(json))
  }

/* Unlike Json_decode.field, this returns None if key is not found */
let fieldOptional = (key, decode, json) =>
  if Js.Json.test(json, Object) {
    let dict: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))
    switch Js.Dict.get(dict, key) {
    | None => None
    /* treat fields with null values as missing fields (atdgen's default) */
    | Some(value) if Js.Json.test(value, Null) => None
    | Some(value) =>
      try Some(with_segment(key, decode, value)) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ ("\n\tat field '" ++ (key ++ "'"))))
      }
    }
  } else {
    None
  }

let fieldDefault = (s, default, f, json) => {
  switch fieldOptional(s, f, json) {
  | None => default
  | Some(s) => s
  }
}

let tuple1 = (f, x) =>
  if Js.Array.isArray(x) {
    let source: array<Js.Json.t> = Obj.magic((x: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 1 {
      try with_segment("0", f, Array.unsafe_get(source, 0)) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple1"))
      }
    } else {
      \"@@"(
        raise,
        DecodeError(`Expected array of length 1, got array of length ${length->Belt.Int.toString}`),
      )
    }
  } else {
    raise(DecodeError("Expected array, got " ++ Js.Json.stringify(x)))
  }

let either = (decoderA, decoderB, json) => {
  try {
    decoderA(json)
  } catch {
  | DecodeError(messageA) =>
    try {
      decoderB(json)
    } catch {
    | DecodeError(messageB) => {
        let formattedErrors = `\n- ${[messageA, messageB]->Js.Array2.joinWith("\n- ")}`
        raise(
          DecodeError(
            `All decoders given to oneOf failed. Here are all the errors:${formattedErrors}\nAnd the JSON being decoded: ${json->Js.Json.stringify}`,
          ),
        )
      }
    }
  }
}

let enum = (l, json) => {
  let constr0 = j => {
    let s = string(j)
    #Constr0(s)
  }

  let constr = j => {
    let p = pair(string, x => x, j)
    #Constr(p)
  }

  switch either(constr0, constr, json) {
  | #Constr0(s) =>
    with_segment(
      s,
      () =>
        switch List.assoc(s, l) {
        | exception Not_found => raise(DecodeError(`unknown constructor "${s}"`))
        | #Single(a) => a
        | #Decode(_) => raise(DecodeError(`constructor "${s}" expects arguments`))
        },
      (),
    )
  | #Constr(s, args) =>
    with_segment(
      s,
      () =>
        switch List.assoc(s, l) {
        | exception Not_found => raise(DecodeError(`unknown constructor "${s}"`))
        | #Single(_) => raise(DecodeError(`constructor "${s}" doesn't expect arguments`))
        | #Decode(d) => decode'(d, args)
        },
      (),
    )
  }
}

let option_as_constr = f =>
  either(
    x =>
      if string(x) == "None" {
        None
      } else {
        raise(DecodeError("Expected None, got " ++ Js.Json.stringify(x)))
      },
    x =>
      switch pair(string, f, x) {
      | ("Some", v) => Some(v)
      | _ => raise(DecodeError("Expected Some _, got " ++ Js.Json.stringify(x)))
      },
  )

let adapter = (normalize: Js.Json.t => Js.Json.t, reader: t<'a>, json) => reader(normalize(json))

let map = (f, decode, json) => json->decode->f

let optional = (decode, json) => {
  try {
    json->decode->Some
  } catch {
  | DecodeError(_) => None
  }
}
