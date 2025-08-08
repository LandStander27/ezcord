use proc_macro::TokenStream;
use quote::quote;
use syn::{
	Data, DeriveInput, FnArg, Ident, Pat, Signature, Visibility,
	parse::{Parse, ParseStream},
	parse_macro_input,
};

struct Function {
	vis: Visibility,
	sig: Signature,
}

impl Parse for Function {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let vis = input.parse()?;
		let sig = input.parse()?;
		// let _semicolon: Token![;] = input.parse()?;

		return Ok(Function { vis, sig });
	}
}

#[proc_macro_derive(DynamicEnum, attributes(call))]
pub fn dynamic_enum(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	let ident = input.ident;

	let out = match input.data {
		Data::Enum(e) => {
			let attributes = input
				.attrs
				.into_iter()
				.filter(|a| a.meta.path().get_ident().unwrap().to_string() == "call");

			let variants = e
				.variants
				.into_iter()
				.map(|v| v.ident)
				.collect::<Vec<Ident>>();

			let mut methods = Vec::new();

			for attr in attributes {
				let call: Function = attr.parse_args().expect("invalid #[call(...)]");
				let vis = call.vis;
				let sig = call.sig;
				let ident = &sig.ident;
				let mut arg_names = Vec::new();
				for arg in &sig.inputs {
					match arg {
						FnArg::Receiver(_recv) => {}
						FnArg::Typed(arg) => match *arg.pat {
							Pat::Ident(ref name) => arg_names.push(name.ident.clone()),
							_ => todo!(),
						},
					}
				}
				let arg_names = std::iter::repeat(arg_names);
				if sig.asyncness.is_none() {
					methods.push(quote! {
						#vis #sig {
							return match self {
								#(
									Self::#variants(val) => val.#ident( #(#arg_names),* )
								),*
							}
						}
					});
				} else {
					methods.push(quote! {
						#vis #sig {
							return match self {
								#(
									Self::#variants(val) => val.#ident( #(#arg_names),* ).await
								),*
							}
						}
					});
				}
			}

			quote! {
				impl #ident {
					#(#methods)*
				}
			}
		}
		_ => todo!(),
	};

	return out.into();
}
