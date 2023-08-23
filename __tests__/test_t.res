/* Auto-generated from "test.atd" */
@@ocaml.warning("-27-32-35-39")

type rec recurse = {recurse_items: list<recurse>}

type rec rec_option = [#Bool | #Nullable(option<rec_option>)]

type rec rec_list = [#Bool | #List(list<rec_list>)]

type rec mutual_recurse1 = {mutual_recurse2: list<mutual_recurse2>}

and mutual_recurse2 = {mutual_recurse1: list<mutual_recurse1>}

type rec container = {id: string, children: list<container>}

type vp = [#A(int) | #B(string)]

type vpl = list<vp>

type v = A(int) | B(string)

type vl = list<v>

type t = (int, string, float)

type myInt = Int64.t

type ro = {c: string, o: option<myInt>}

type r = {a: int, b: string}

type optional_field = {
  with_default: int,
  no_default: option<int>,
  no_default_nullable: option<int>,
}

type n = option<int>

type an_array = Atdgen_runtime.Util.ocaml_array<int>

type deeply_nested = [#A(an_array, rec_list)]

type b = {thing: int}

type adapted_scalar = [#A(int) | #B(string)]

type adapted_list = [#A(list<int>)]

type a = {thing: string, other_thing: bool}

type adapted_kind = [#A(a) | #B(b)]

type adapted = [#A(a) | #B(b)]
