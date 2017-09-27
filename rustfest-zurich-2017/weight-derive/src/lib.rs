#![feature(proc_macro)]
extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_derive(Weight)]
pub fn weight(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();

    let gen = impl_weight(&ast);
    println!("deriving {}", gen);
    gen.parse().unwrap()
}

#[proc_macro]
pub fn impl_weight_for(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();

    let gen = impl_weight(&ast);
    println!("generating {}", gen);
    gen.parse().unwrap()
}

fn impl_weight(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = &ast.ident;
    let weight_body = match ast.body {
        syn::Body::Struct(ref variant_data) =>
            generate_match_for_cases(::std::iter::once((&ast.ident, variant_data))),
        syn::Body::Enum(ref variants) =>
            generate_match_for_cases(variants.iter().map(|v| &v.ident).zip(variants.iter().map(|v| &v.data)))
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
        impl #impl_params Weight for #name #ty_args where #where_clause #added_bounds {
            fn weight(&self) -> isize { #weight_body }
        }
    };

    fn generate_match_for_cases<'a, I>(iter: I) -> quote::Tokens
        where I: Iterator<Item=(&'a syn::Ident, &'a syn::VariantData)>
    {
        let mut arms = quote! { };
        for (variant_ident, variant_data) in iter {
            let (field_names, code) = generate_variant_data_code(variant_data);
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

    fn generate_variant_data_code(vd: &syn::VariantData) -> (Vec<quote::Tokens>, quote::Tokens)
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
}
