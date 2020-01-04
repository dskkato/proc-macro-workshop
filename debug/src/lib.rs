extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    self, parse_macro_input, parse_quote, DeriveInput, GenericParam, Generics, Lit, Meta,
    MetaNameValue,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident.clone();
    let struct_ident = format!("{}", ident);

    let data = &input.data;

    let fields = match *data {
        syn::Data::Struct(ref data) => match data.fields {
            syn::Fields::Named(ref fields) => fields,
            syn::Fields::Unnamed(_) | syn::Fields::Unit => unimplemented!(),
        },
        syn::Data::Enum(_) | syn::Data::Union(_) => unimplemented!(),
    };

    let phantom_data_ty: std::collections::HashSet<syn::Ident> = fields
        .named
        .iter()
        .filter_map(|f| {
            // let ident = &f.ident.clone().unwrap();
            let segment = match &f.ty {
                syn::Type::Path(ty) => &ty.path.segments[0],
                _ => return None,
            };
            if segment.ident != "PhantomData" {
                return None;
            }

            let argument = match &segment.arguments {
                syn::PathArguments::AngleBracketed(bracketed) => &bracketed.args[0],
                _ => return None,
            };
            match argument {
                syn::GenericArgument::Type(syn::Type::Path(arg)) => {
                    Some(arg.path.segments[0].ident.clone())
                }
                _ => None,
            }
        })
        .collect();

    let struct_fields = fields.named.iter().map(|f| {
        let ident = &f.ident.clone().unwrap();
        let ident_str = format!("{}", ident);
        match get_format(&f) {
            Some(fmt) => quote! { field(#ident_str, &format_args!(#fmt, &self.#ident)) },
            None => quote! { field(#ident_str, &self.#ident) },
        }
    });

    let generics = add_trait_bounds(input.generics, phantom_data_ty);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #ident #ty_generics #where_clause{
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_ident)
                    #(.#struct_fields)*
                    .finish()
            }
        }
    };

    expanded.into()
}

fn get_format(field: &syn::Field) -> Option<String> {
    let attr = field.attrs.first();
    match attr {
        Some(attr) => {
            let meta = attr.parse_meta();
            match meta {
                Ok(Meta::NameValue(MetaNameValue {
                    path,
                    lit: Lit::Str(lit_str),
                    ..
                })) => {
                    assert!(path.is_ident("debug"));
                    Some(lit_str.value())
                }
                _ => unimplemented!(),
            }
        }
        _ => None,
    }
}

// Add a bound `T: HeapSize` to every type parameter T.
fn add_trait_bounds(
    mut generics: Generics,
    excepts: std::collections::HashSet<syn::Ident>,
) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if !excepts.contains(&type_param.ident) {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics
}
