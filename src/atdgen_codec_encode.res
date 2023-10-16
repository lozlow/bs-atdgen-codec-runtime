include JsonCombinators.Json_Encode

let make = f => f

let encode = (f, x) => f(x)

let unit = () => JsonCombinators.Json_Encode.null

let int32 = s => string(Int32.to_string(s))

let int64 = s => string(Int64.to_string(s))

let nullable = (encode, val) =>
  switch val {
  | None => JsonCombinators.Json_Encode.null
  | Some(val) => encode(val)
  }

let char = char => String.make(1, char)->string

type spec<'a, 'b> = {name: string, data: 'a, encode: t<'b>}

type field_spec<'a> =
  | Optional(spec<option<'a>, 'a>, option<'a>)
  | Required(spec<'a, 'a>, option<'a>)

type rec field = F(field_spec<'a>): field

let list = (encode, l) => l |> Array.of_list |> array(encode)

let field = (~default=?, encode, ~name, data) => F(Required({name, data, encode}, default))

let field_o = (~default=?, encode, ~name, data) => F(Optional({name, data, encode}, default))

let obj = fields => List.fold_left((acc, F(f)) =>
    switch f {
    | Required({name, data, encode}, None) => list{(name, encode(data)), ...acc}
    | Required({name, data, encode}, Some(default)) =>
      if default == data {
        acc
      } else {
        list{(name, encode(data)), ...acc}
      }
    | Optional({name, data, encode}, default) =>
      switch (data, default) {
      | (None, _) => acc
      | (Some(s), Some(default)) =>
        if s == default {
          acc
        } else {
          list{(name, encode(s)), ...acc}
        }
      | (Some(s), None) => list{(name, encode(s)), ...acc}
      }
    }
  , list{}, fields)->Array.of_list->object

let tuple1 = (f, x) => jsonArray([f(x)])

let contramap = (f, g, b) => g(f(b))

let constr0 = string

let constr1 = (s, f, x) => pair(string, f, (s, x))

let option_as_constr = (f, x) =>
  switch x {
  | None => string("None")
  | Some(s) => pair(string, f, ("Some", s))
  }

let adapter = (restore: Js.Json.t => Js.Json.t, writer: t<'a>, x) => {
  let encoded = writer(x)
  restore(encoded)
}
