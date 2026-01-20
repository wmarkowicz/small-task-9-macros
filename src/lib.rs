use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Lit, Meta, NestedMeta,
    PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match impl_builder(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn impl_builder(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let builder_name = syn::Ident::new(&format!("{}Builder", name), name.span());

    let Data::Struct(data_struct) = &input.data else {
        return Err(syn::Error::new_spanned(
            input,
            "Builder can only be derived for structs",
        ));
    };

    let Fields::Named(fields_named) = &data_struct.fields else {
        return Err(syn::Error::new_spanned(
            input,
            "Builder can only be derived for structs with named fields",
        ));
    };

    let mut builder_fields = Vec::new();
    let mut initializers = Vec::new();
    let mut setter_methods = Vec::new();
    let mut build_fields = Vec::new();

    for field in &fields_named.named {
        let field_name = field.ident.as_ref().unwrap();
        let field_type = &field.ty;

        // Check if it's Option<T>
        let is_option = is_option_type(field_type);
        let inner_type = if is_option {
            get_option_inner_type(field_type)
        } else {
            None
        };

        // Check for builder attributes
        let mut each_method_name = None;
        for attr in &field.attrs {
            if attr.path.is_ident("builder") {
                let meta = attr.parse_meta()?;

                match meta {
                    Meta::List(meta_list) => {
                        for nested in &meta_list.nested {
                            match nested {
                                NestedMeta::Meta(Meta::NameValue(name_value)) => {
                                    if name_value.path.is_ident("each") {
                                        if let Lit::Str(lit_str) = &name_value.lit {
                                            each_method_name = Some(lit_str.value());
                                        }
                                    } else {
                                        return Err(syn::Error::new_spanned(
                                            &name_value.path,
                                            "expected `builder(each = \"...\")`",
                                        ));
                                    }
                                }
                                _ => {
                                    return Err(syn::Error::new_spanned(
                                        nested,
                                        "expected `builder(each = \"...\")`",
                                    ));
                                }
                            }
                        }
                    }
                    Meta::Path(path) => {
                        // Handle #[builder] without arguments
                        return Err(syn::Error::new_spanned(
                            path,
                            "expected `builder(each = \"...\")`",
                        ));
                    }
                    Meta::NameValue(name_value) => {
                        return Err(syn::Error::new_spanned(
                            &name_value,
                            "expected `builder(each = \"...\")`",
                        ));
                    }
                }
            }
        }

        // For Option<T> fields, builder field should be Option<T>
        let builder_field_type = if is_option {
            quote! { #field_type }
        } else {
            quote! { ::std::option::Option<#field_type> }
        };

        builder_fields.push(quote! {
            #field_name: #builder_field_type
        });

        initializers.push(quote! {
            #field_name: ::std::option::Option::None
        });

        // Handle each-style setters for Vec fields
        if let Some(each_name) = each_method_name {
            // Get the inner type of Vec<T>
            if let Some(inner_vec_type) = get_vec_inner_type(field_type) {
                let each_ident = syn::Ident::new(&each_name, field_name.span());

                // Generate method to add single element
                setter_methods.push(quote! {
                    pub fn #each_ident(&mut self, value: #inner_vec_type) -> &mut Self {
                        match &mut self.#field_name {
                            ::std::option::Option::Some(vec) => {
                                vec.push(value);
                            }
                            ::std::option::Option::None => {
                                let mut vec = ::std::vec::Vec::new();
                                vec.push(value);
                                self.#field_name = ::std::option::Option::Some(vec);
                            }
                        }
                        self
                    }
                });

                // Also generate a regular setter that takes the whole vector
                setter_methods.push(quote! {
                    pub fn #field_name(&mut self, value: #field_type) -> &mut Self {
                        self.#field_name = ::std::option::Option::Some(value);
                        self
                    }
                });
            } else {
                return Err(syn::Error::new_spanned(
                    field_type,
                    "`each` attribute can only be used on Vec fields",
                ));
            }
        } else {
            // Regular setter method
            if is_option {
                // For Option fields, setter should accept T, not Option<T>
                if let Some(inner_ty) = inner_type {
                    setter_methods.push(quote! {
                        pub fn #field_name(&mut self, value: #inner_ty) -> &mut Self {
                            self.#field_name = ::std::option::Option::Some(value);
                            self
                        }
                    });
                }
            } else {
                setter_methods.push(quote! {
                    pub fn #field_name(&mut self, value: #field_type) -> &mut Self {
                        self.#field_name = ::std::option::Option::Some(value);
                        self
                    }
                });
            }
        }

        // Build field logic
        if is_option {
            // For Option fields, we can use take() directly
            build_fields.push(quote! {
                #field_name: self.#field_name.take()
            });
        } else {
            build_fields.push(quote! {
                #field_name: self.#field_name.take().ok_or_else(|| {
                    ::std::boxed::Box::<dyn ::std::error::Error>::from(
                        format!("field `{}` was not set", stringify!(#field_name))
                    )
                })?
            });
        }
    }

    let result = quote! {
        pub struct #builder_name {
            #(#builder_fields,)*
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#initializers,)*
                }
            }
        }

        impl #builder_name {
            #(#setter_methods)*

            pub fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                Ok(#name {
                    #(#build_fields,)*
                })
            }
        }
    };

    Ok(result)
}

fn is_option_type(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.first() {
            if segment.ident == "Option" {
                return true;
            }
        }
    }
    false
}

fn get_option_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.first() {
            if segment.ident == "Option" {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}

fn get_vec_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.first() {
            if segment.ident == "Vec" {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}
