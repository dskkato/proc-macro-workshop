extern crate proc_macro;

use proc_macro::TokenStream as StdTokenStream;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    self, parse_macro_input, Data, DeriveInput, Field, Fields, Lit, Meta, NestedMeta, Result,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: StdTokenStream) -> StdTokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let data = &input.data;

    let fields = match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => fields,
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    let syntax_check = fields.named.iter().map(|f| check_syntax(&f));
    for v in syntax_check {
        match v {
            Err(e) => return e.to_compile_error().into(),
            _ => (),
        };
    }
    let expanded_fields = fields.named.iter().map(|f| get_expanded_fields(&f));
    let expanded_fn = fields.named.iter().map(|f| get_expanded_fn(&f));
    let initializer = fields.named.iter().map(|f| get_initializer(&f));
    let builder = fields.named.iter().map(|f| get_builder(&f));

    let name = &input.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = syn::Ident::new(&builder_name, name.span());

    let mut expanded = Vec::new();
    expanded.push(quote! {
        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#initializer,)*
                }
            }
        }
    });

    expanded.push(quote! {
        pub struct #builder_ident {
            #(#expanded_fields,)*
        }
    });

    expanded.push(quote!{
        impl #builder_ident {
            pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(
                    #name {
                        #(#builder,)*
                    }
                )
            }

            #(#expanded_fn)*
        }
    });

    expanded.into_iter().collect::<TokenStream>().into()
}

fn get_inner_type(wrapper: &str, ty: &syn::Type) -> std::option::Option<syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return std::option::Option::None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return std::option::Option::None;
            }
            // can unwrap because we already checked the length
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return std::option::Option::Some(t.clone());
            }
        }
    }
    std::option::Option::None
}

fn get_each(f: &Field) -> std::option::Option<Result<syn::Path>> {
    let attr = &f.attrs.first();
    let attr = match attr {
        Some(v) => v,
        _ => return std::option::Option::None,
    };

    if !attr.path.is_ident("builder") {
        return std::option::Option::None;
    };

    let meta = match attr.parse_meta() {
        Ok(v) => v,
        _ => return std::option::Option::None,
    };

    if let Meta::List(meta_list) = meta {
        if let Some(NestedMeta::Meta(Meta::NameValue(nv))) = meta_list.nested.first() {
            if nv.path.is_ident("each") {
                if let Lit::Str(lit_str) = &nv.lit {
                    return std::option::Option::Some(Ok(lit_str.parse().unwrap()));
                }
            }
            return std::option::Option::Some(Err(syn::Error::new_spanned(
                meta_list,
                "expected `builder(each = \"...\")`",
            )));
        }
    }
    std::option::Option::None
}

fn get_expanded_fields(f: &Field) -> proc_macro2::TokenStream {
    let ident = &f.ident.clone().unwrap();
    let ty = &f.ty;

    if get_inner_type("Option", &f.ty).is_some() {
        quote! {
            #ident: #ty
        }
    } else {
        let each = get_each(&f);
        match each {
            Some(Ok(_)) => {
                quote! {
                    #ident: #ty
                }
            }
            Some(Err(e)) => e.to_compile_error(),
            None => {
                quote! {
                    #ident: std::option::Option<#ty>
                }
            }
        }
    }
}

fn check_syntax(f: &Field) -> Result<TokenStream> {
    let ident = &f.ident.clone().unwrap();
    let ty = &f.ty;
    let each = get_each(&f);
    match each {
        Some(Ok(_)) => Ok(quote! {
            #ident: <#ty>::new()
        }),
        Some(Err(e)) => Err(e),
        None => Ok(quote! {
            #ident: std::option::Option::None
        }),
    }
}

fn get_initializer(f: &Field) -> TokenStream {
    let ident = &f.ident.clone().unwrap();
    let ty = &f.ty;
    let each = get_each(&f);

    match each {
        Some(Ok(_)) => {
            quote! {
                #ident: <#ty>::new()
            }
        }
        Some(Err(e)) => e.to_compile_error(),
        None => {
            quote! {
                #ident: std::option::Option::None
            }
        }
    }
}

fn get_builder(f: &Field) -> TokenStream {
    let ident = &f.ident.clone().unwrap();
    let each = get_each(&f);

    if get_inner_type("Option", &f.ty).is_some() {
        match each {
            Some(Err(e)) => e.to_compile_error(),
            _ => {
                quote! {
                    #ident: self.#ident.clone()
                }
            }
        }
    } else if get_inner_type("Vec", &f.ty).is_some() {
        match each {
            Some(Ok(_)) => {
                quote! {
                //    #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), "is not set"))?
                   #ident: self.#ident.clone()
                }
            }
            _ => {
                quote! {
                   #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), "is not set"))?
                }
            }
        }
    } else {
        quote! {
            #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), "is not set"))?
        }
    }
}

fn get_expanded_fn(f: &Field) -> TokenStream {
    let ident = &f.ident.clone().unwrap();
    let ty = &f.ty;

    let each = get_each(&f);
    let inner_ty_should_vec = get_inner_type("Vec", &f.ty);
    match each {
        Some(Err(e)) => e.to_compile_error(),
        Some(Ok(each_)) => match inner_ty_should_vec {
            Some(inner_ty) => {
                quote! {
                    pub fn #each_(&mut self, #ident: #inner_ty) -> &mut Self {
                        self.#ident.push(#ident.clone());
                        self
                    }
                }
            }
            _ => panic!("should not be called"),
        },
        _ => {
            let inner_ty_should_option = get_inner_type("Option", &f.ty);
            match inner_ty_should_option {
                std::option::Option::Some(tyty) => {
                    quote! {
                        pub fn #ident(&mut self, #ident: #tyty) -> &mut Self {
                            self.#ident = std::option::Option::Some(#ident.clone());
                            self
                        }
                    }
                }
                std::option::Option::None => {
                    quote! {
                        pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = std::option::Option::Some(#ident);
                            self
                        }
                    }
                }
            }
        }
    }
}
