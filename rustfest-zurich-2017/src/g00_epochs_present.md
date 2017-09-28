# The Second of the Three Spirits

## Procedural macros, custom `#[derive]`

```rust
/// Measure number of heap-allocated values owned by self.
trait Weight {
    // Base assumption: most values are allocated inline, not a separate `malloc`
    fn weight(&self) -> isize { 0 }
}
```

Some base cases:

```rust
impl Weight for i32 { }
impl Weight for i64 { }
// (et cetera)

impl<'a, T: ?Sized> Weight for &'a T { } // someone else owns this, not our problem.
impl<'a, T: ?Sized> Weight for &'a mut T { } // ditto
// (aside: `?Sized` marker is to allow things like `str` or `[i32]` in for `T`.)
```

## The first interesting cases

Heap-allocated things like `String` and `Box` must be accounted for.

```rust
impl Weight for String {
    fn weight(&self) -> isize { 1 } // just a single heap allocation
}
```

`Box` has a heap allocation ...

<p class="fragment">*plus* its contents may own others!</p>

. . .

```rust
impl<T: Weight> Weight for Box<T> {
    fn weight(&self) -> isize { 1 + (**self).weight() }
}

#[test]
fn test_box_chain() {
    assert_eq!( Box::new(Box::new(Box::new("hi"))).weight(), 3 );
}
```

----

`Vec` is similar to `Box`: pay for own heap allocation, plus those of contents.

```rust
impl<T: Weight> Weight for Vec<T> {
    fn weight(&self) -> isize {
        1 + self.iter().map(|x|x.weight()).sum::<isize>()
    }
}

#[test]
fn test_vec_of_boxes() {
    assert_eq!( vec![Box::new(1), Box::new(2), Box::new(3)].weight(), 4 );
}

#[test]
fn test_option_box() {
    assert_eq!( Some(Box::new(1)).weight(), 1 );
}
```

## The first annoying cases

``` {.rust}
impl<T: Weight> Weight for Option<T> {
    fn weight(&self) -> isize {
        match *self {
            Some(ref t) => t.weight(),
            None => 0,
        }
    }
}
```

(bog standard recursion)

. . .

``` {.rust}
impl<T: Weight, E: Weight> Weight for Result<T, E> {
    fn weight(&self) -> isize {
        match *self {
            Ok(ref t) => t.weight(),
            Err(ref e) => e.weight(),
        }
    }
}
```

(yadda yadda yadda)

----

Implementations for `Option<T>` and `Result<T, E>` were entirely mechanical,
derived from the structure of the type.

  . . .

In fact, one might suggest that a program could generate them for us.

 . . .

So lets do that

(working from the hypothesis that its easier to transcribe a type
definition into a macro invocation...)


```rust
use weight_derive::impl_weight_for;
impl_weight_for!(enum Option<T> { Some(T), None });
```

yields (after some human adjustment of whitespace):

``` {.rust}
impl<T> Weight for Option<T> where T: Weight, {
    fn weight (&self) -> isize { match *self {
        Some (ref x_0) => 0 + x_0.weight(), None => 0, } } }
```

 . . .

(Okay, less readable than previous slide. But who cares, its a macro!)

## What does the macro *implementation* look like?

You don't want to see it.

----

Since you asked

<div style="margin: 0 auto;">
<div style="float: left; width: 50%">

``` {.rust .halfsize}
#![feature(proc_macro)]
extern crate proc_macro;
extern crate syn;
#[macro_use] extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_derive(Weight)]
pub fn weight(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = impl_weight(&ast);
    gen.parse().unwrap()
}

#[proc_macro]
pub fn impl_weight_for(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = impl_weight(&ast);
    gen.parse().unwrap()
}

fn impl_weight(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = &ast.ident;
    let weight_body = match ast.body {
        syn::Body::Struct(ref variant_data) =>
            generate_match_for_cases(
                ::std::iter::once((&ast.ident, variant_data))),
        syn::Body::Enum(ref variants) =>
            generate_match_for_cases(
                variants.iter().map(|v| &v.ident)
                    .zip(variants.iter().map(|v| &v.data)))
    };
    let (impl_params, ty_args, where_clause) = ast.generics.split_for_impl();
    let mut added_bounds: quote::Tokens = quote! { };
    for bound in ast.generics.ty_params.iter()
        .map(|ty_param| &ty_param.ident )
        .map(|ident| quote! { #ident: Weight, })
    {
        added_bounds = quote! { #added_bounds #bound };
    }
    return quote! {
        impl #impl_params Weight for #name #ty_args 
            where #where_clause #added_bounds {
                fn weight(&self) -> isize { #weight_body }
        }
    };
}
```

</div>
<div style="float: left; width: 50%">

``` {.rust .halfsize}
fn generate_match_for_cases<'a, I>(iter: I) -> quote::Tokens
    where I: Iterator<Item=(&'a syn::Ident, &'a syn::VariantData)>
{
    let mut arms = quote! { };
    for (variant_ident, variant_data) in iter {
        let (field_names, code) = generate_variant_code(variant_data);
        let arm = match *variant_data {
            syn::VariantData::Unit =>
                quote! { #variant_ident => #code },
            syn::VariantData::Struct(..) =>
                quote! { #variant_ident { #(ref #field_names),* } => #code },
            syn::VariantData::Tuple(..) =>
                quote! { #variant_ident(#(ref #field_names),*) => #code },
        };
        arms = quote! { #arms #arm, }
    }
    return quote! { match *self { #arms } };
}

fn generate_variant_code(vd: &syn::VariantData) -> (Vec<quote::Tokens>, quote::Tokens)
{
    let bind_names: Vec<_> = match *vd {
        syn::VariantData::Unit => vec![],
        syn::VariantData::Struct(ref fields) => fields.iter()
            .map(|f|f.ident.as_ref().unwrap()) // reuse names for struct fields
            .map(|i| quote!{ #i })
            .collect(),
        syn::VariantData::Tuple(ref fields) => fields.iter()
            .enumerate() // make fresh names for tuple fields
            .map(|(num, _)| { quote::Ident::new(format!("x_{}", num)) })
            .map(|i| quote!{ #i })
            .collect(),
    };
    let code = generate_sum_of_var_weights(&bind_names);
    (bind_names, code)
}

fn generate_sum_of_var_weights(names: &[quote::Tokens]) -> quote::Tokens {
    let mut body = quote! { 0 };
    for field_access in names {
        body = quote! { #body + #field_access .weight() };
    }
    return body;
}
```

</div>
</div>

. . .

<div style="float: right">
(Told you so!)
</div>

## Sad News

Procedural macros like `impl_weight_for!` are not yet
available in the Rust stable channel...

. . .

... *except* for one special case: `#[derive]`!

## Stable custom #[derive]

<!--
`#[derive(Macro)] item_defn` looks up a procedural macro definition
(carrying an attribute of form `#[proc_macro_derive(Macro)]`) and then
feeds the AST for `item_defn` into that macro.
-->

```rust
#[derive(Weight)] struct FrenchToast;
```

yields:

``` {.rust}
impl Weight for FrenchToast where {
    fn weight (&self) -> isize { match *self { FrenchToast => 0, } } }
```

. . .

```rust
#[derive(Weight)]
struct Pancakes { topping: &'static str, atop: Option<Box<Pancakes>>, }
```

yields:

``` {.rust}
impl Weight for Pancakes where {
    fn weight (&self) -> isize {
        match *self {
            Pancakes { ref topping, ref atop } => 0 + topping.weight() + atop.weight(),
        }
    }
}
```

## Over Weight Demo {.no_margin}

```rust
pub fn breakfast() {
    let f = FrenchToast;
    assert_eq!(f.weight(), 0);
    let p1 = Pancakes { topping: "syrup", atop: None };
```

(Pop quiz: what's `p1.weight()`?)

<div class="fragment">

```rust
    assert_eq!(p1.weight(), 0);
```

</div>

```rust
    let p3 = Pancakes { topping: "butter",
                        atop: Some(Box::new(
                            Pancakes { topping: "berries",
                                       atop: Some(Box::new(
                                           Pancakes { topping: "syrup",
                                                      atop: None })) }))};
```

(How about `p3.weight()`?)

<div class="fragment">

```rust
    assert_eq!(p3.weight(), 2);
```

</div>

```rust
}
```

<!--
```rust
#[test]
pub fn test_breakfast() { breakfast() }
pub fn main() { breakfast() }
```
-->
