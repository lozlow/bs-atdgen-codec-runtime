/* Auto-generated from "test.atd" */
@@ocaml.warning("-27-32-35-39")

type rec recurse = Test_t.recurse = {recurse_items: list<recurse>}

type rec_option = Test_t.rec_option

type rec_list = Test_t.rec_list

type rec mutual_recurse1 = Test_t.mutual_recurse1 = {mutual_recurse2: list<mutual_recurse2>}

and mutual_recurse2 = Test_t.mutual_recurse2 = {mutual_recurse1: list<mutual_recurse1>}

type rec container = Test_t.container = {id: string, children: list<container>}

type vp = Test_t.vp

type vpl = Test_t.vpl

type v = Test_t.v = A(int) | B(string)

type vl = Test_t.vl

type t = Test_t.t

type myInt = Test_t.myInt

type ro = Test_t.ro = {c: string, o: option<myInt>}

type r = Test_t.r = {a: int, b: string}

type optional_field = Test_t.optional_field = {
  with_default: int,
  no_default: option<int>,
  no_default_nullable: option<int>,
}

type n = Test_t.n

type an_array = Test_t.an_array

type deeply_nested = Test_t.deeply_nested

type b = Test_t.b = {thing: int}

type adapted_scalar = Test_t.adapted_scalar

type adapted_list = Test_t.adapted_list

type a = Test_t.a = {thing: string, other_thing: bool}

type adapted_kind = Test_t.adapted_kind

type adapted = Test_t.adapted

let read_recurse: Atdgen_codec_runtime.Decode.t<recurse>

let write_recurse: Atdgen_codec_runtime.Encode.t<recurse>

let read_rec_option: Atdgen_codec_runtime.Decode.t<rec_option>

let write_rec_option: Atdgen_codec_runtime.Encode.t<rec_option>

let read_rec_list: Atdgen_codec_runtime.Decode.t<rec_list>

let write_rec_list: Atdgen_codec_runtime.Encode.t<rec_list>

let read_mutual_recurse1: Atdgen_codec_runtime.Decode.t<mutual_recurse1>

let write_mutual_recurse1: Atdgen_codec_runtime.Encode.t<mutual_recurse1>

let read_mutual_recurse2: Atdgen_codec_runtime.Decode.t<mutual_recurse2>

let write_mutual_recurse2: Atdgen_codec_runtime.Encode.t<mutual_recurse2>

let read_container: Atdgen_codec_runtime.Decode.t<container>

let write_container: Atdgen_codec_runtime.Encode.t<container>

let read_vp: Atdgen_codec_runtime.Decode.t<vp>

let write_vp: Atdgen_codec_runtime.Encode.t<vp>

let read_vpl: Atdgen_codec_runtime.Decode.t<vpl>

let write_vpl: Atdgen_codec_runtime.Encode.t<vpl>

let read_v: Atdgen_codec_runtime.Decode.t<v>

let write_v: Atdgen_codec_runtime.Encode.t<v>

let read_vl: Atdgen_codec_runtime.Decode.t<vl>

let write_vl: Atdgen_codec_runtime.Encode.t<vl>

let read_t: Atdgen_codec_runtime.Decode.t<t>

let write_t: Atdgen_codec_runtime.Encode.t<t>

let read_myInt: Atdgen_codec_runtime.Decode.t<myInt>

let write_myInt: Atdgen_codec_runtime.Encode.t<myInt>

let read_ro: Atdgen_codec_runtime.Decode.t<ro>

let write_ro: Atdgen_codec_runtime.Encode.t<ro>

let read_r: Atdgen_codec_runtime.Decode.t<r>

let write_r: Atdgen_codec_runtime.Encode.t<r>

let read_optional_field: Atdgen_codec_runtime.Decode.t<optional_field>

let write_optional_field: Atdgen_codec_runtime.Encode.t<optional_field>

let read_n: Atdgen_codec_runtime.Decode.t<n>

let write_n: Atdgen_codec_runtime.Encode.t<n>

let read_an_array: Atdgen_codec_runtime.Decode.t<an_array>

let write_an_array: Atdgen_codec_runtime.Encode.t<an_array>

let read_deeply_nested: Atdgen_codec_runtime.Decode.t<deeply_nested>

let write_deeply_nested: Atdgen_codec_runtime.Encode.t<deeply_nested>

let read_b: Atdgen_codec_runtime.Decode.t<b>

let write_b: Atdgen_codec_runtime.Encode.t<b>

let read_adapted_scalar: Atdgen_codec_runtime.Decode.t<adapted_scalar>

let write_adapted_scalar: Atdgen_codec_runtime.Encode.t<adapted_scalar>

let read_adapted_list: Atdgen_codec_runtime.Decode.t<adapted_list>

let write_adapted_list: Atdgen_codec_runtime.Encode.t<adapted_list>

let read_a: Atdgen_codec_runtime.Decode.t<a>

let write_a: Atdgen_codec_runtime.Encode.t<a>

let read_adapted_kind: Atdgen_codec_runtime.Decode.t<adapted_kind>

let write_adapted_kind: Atdgen_codec_runtime.Encode.t<adapted_kind>

let read_adapted: Atdgen_codec_runtime.Decode.t<adapted>

let write_adapted: Atdgen_codec_runtime.Encode.t<adapted>
