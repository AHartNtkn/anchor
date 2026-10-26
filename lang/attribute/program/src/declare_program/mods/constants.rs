use {
    super::common::{convert_idl_type_to_str, gen_docs},
    anchor_lang_idl::types::{Idl, IdlType},
    quote::{format_ident, quote, ToTokens},
    syn::visit_mut::VisitMut,
};

pub fn gen_constants_mod(idl: &Idl) -> proc_macro2::TokenStream {
    let defined_paths = get_defined_paths(idl);
    let constants = idl.constants.iter().map(|c| {
        let name = format_ident!("{}", c.name);
        let docs = gen_docs(&c.docs);
        #[allow(
            clippy::unwrap_used,
            reason = "compile_error! token stream is always valid syn::Type syntax"
        )]
        let ty = convert_idl_type_to_str(&c.ty, true)
            .and_then(|s| {
                syn::parse_str::<syn::Type>(&s)
                    .map_err(|err| syn::Error::new(proc_macro2::Span::call_site(), err.to_string()))
            })
            .unwrap_or_else(|err| syn::parse2(err.into_compile_error()).unwrap());
        #[allow(
            clippy::unwrap_used,
            reason = "IDL constant values are valid Rust expressions by construction"
        )]
        let mut val = syn::parse_str::<syn::Expr>(&c.value).unwrap();
        DefinedPathQualifier::new(&defined_paths).visit_expr_mut(&mut val);
        let val = val.to_token_stream();
        let val = match &c.ty {
            IdlType::Bytes => quote! { &#val },
            IdlType::Pubkey => quote!(anchor_lang::prelude::Pubkey::from_str_const(
                stringify!(#val)
            )),
            IdlType::U256 | IdlType::I256 => {
                let signed = matches!(c.ty, IdlType::I256);
                match wide_int_le_bytes(&c.value, signed) {
                    Some(bytes) => quote! { [#(#bytes),*] },
                    None => syn::Error::new(
                        proc_macro2::Span::call_site(),
                        format!(
                            "constant `{}`: `{}` is not a {} value",
                            c.name,
                            c.value,
                            if signed { "i256" } else { "u256" }
                        ),
                    )
                    .into_compile_error(),
                }
            }
            _ => val,
        };

        quote! {
            #docs
            pub const #name: #ty = #val;
        }
    });

    quote! {
        /// Program constants.
        pub mod constants {
            use super::*;

            #(#constants)*
        }
    }
}

/// The 32 little-endian bytes of the decimal integer `value`, in two's
/// complement when `signed`, or `None` when it is not an integer of that
/// type.
fn wide_int_le_bytes(value: &str, signed: bool) -> Option<[u8; 32]> {
    let value = value.trim();
    let (negative, digits) = match value.strip_prefix('-') {
        Some(_) if !signed => return None,
        Some(digits) => (true, digits),
        None => (false, value),
    };
    let digits = digits.replace('_', "");
    if digits.is_empty() {
        return None;
    }
    let mut bytes = [0u8; 32];
    for digit in digits.chars() {
        let mut carry = digit.to_digit(10)?;
        for byte in bytes.iter_mut() {
            let wide = u32::from(*byte) * 10 + carry;
            *byte = (wide & 0xff) as u8;
            carry = wide >> 8;
        }
        if carry != 0 {
            return None;
        }
    }
    if !signed {
        return Some(bytes);
    }
    let sign_bit = |bytes: &[u8; 32]| bytes.last().is_some_and(|&top| top & 0x80 != 0);
    if negative {
        // Negate: invert, then add one.
        for byte in bytes.iter_mut() {
            *byte = !*byte;
        }
        for byte in bytes.iter_mut() {
            let (sum, overflow) = byte.overflowing_add(1);
            *byte = sum;
            if !overflow {
                break;
            }
        }
        // A magnitude above 2^255 wraps to a non-negative value.
        let is_zero = bytes.iter().all(|&byte| byte == 0);
        (is_zero || sign_bit(&bytes)).then_some(bytes)
    } else {
        (!sign_bit(&bytes)).then_some(bytes)
    }
}

fn get_defined_paths(idl: &Idl) -> Vec<Vec<String>> {
    let mut paths = idl
        .accounts
        .iter()
        .map(|acc| acc.name.as_str())
        .chain(idl.events.iter().map(|event| event.name.as_str()))
        .chain(idl.types.iter().map(|ty| ty.name.as_str()))
        .map(|name| name.split("::").map(str::to_owned).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    paths.sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
    paths.dedup();
    paths
}

struct DefinedPathQualifier<'a> {
    defined_paths: &'a [Vec<String>],
}

impl<'a> DefinedPathQualifier<'a> {
    fn new(defined_paths: &'a [Vec<String>]) -> Self {
        Self { defined_paths }
    }

    fn qualify_path(&self, path: &mut syn::Path) {
        if path.leading_colon.is_some() || path.segments.is_empty() {
            return;
        }

        let Some(first_segment) = path.segments.first() else {
            return;
        };
        let first = first_segment.ident.to_string();
        if matches!(first.as_str(), "__defined" | "crate" | "self" | "super") {
            return;
        }

        let path_idents = path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>();
        let should_qualify = self
            .defined_paths
            .iter()
            .any(|defined| path_idents.starts_with(defined));
        if !should_qualify {
            return;
        }

        let old_segments = path.segments.clone();
        path.segments = syn::punctuated::Punctuated::new();
        path.segments.push(format_ident!("__defined").into());
        path.segments.extend(old_segments);
    }
}

impl VisitMut for DefinedPathQualifier<'_> {
    fn visit_expr_path_mut(&mut self, expr_path: &mut syn::ExprPath) {
        syn::visit_mut::visit_expr_path_mut(self, expr_path);
        if expr_path.qself.is_none() {
            self.qualify_path(&mut expr_path.path);
        }
    }

    fn visit_expr_struct_mut(&mut self, expr_struct: &mut syn::ExprStruct) {
        syn::visit_mut::visit_expr_struct_mut(self, expr_struct);
        self.qualify_path(&mut expr_struct.path);
    }
}

#[cfg(test)]
#[allow(
    clippy::indexing_slicing,
    reason = "tests index fixed 32-byte arrays at known positions"
)]
mod tests {
    use super::wide_int_le_bytes;

    const U256_MAX: &str =
        "115792089237316195423570985008687907853269984665640564039457584007913129639935";
    const U256_MAX_PLUS_ONE: &str =
        "115792089237316195423570985008687907853269984665640564039457584007913129639936";
    const I256_MAX: &str =
        "57896044618658097711785492504343953926634992332820282019728792003956564819967";
    const I256_MIN: &str =
        "-57896044618658097711785492504343953926634992332820282019728792003956564819968";

    fn le(low: &[u8], fill: u8) -> [u8; 32] {
        let mut bytes = [fill; 32];
        bytes
            .iter_mut()
            .zip(low)
            .for_each(|(byte, &value)| *byte = value);
        bytes
    }

    #[test]
    fn u256_values_are_little_endian() {
        assert_eq!(wide_int_le_bytes("0", false), Some([0; 32]));
        assert_eq!(wide_int_le_bytes("258", false), Some(le(&[2, 1], 0)));
        assert_eq!(
            wide_int_le_bytes("1_000", false),
            Some(le(&[0xe8, 0x03], 0))
        );
        assert_eq!(wide_int_le_bytes(U256_MAX, false), Some([0xff; 32]));
    }

    #[test]
    fn i256_values_are_twos_complement() {
        assert_eq!(wide_int_le_bytes("-1", true), Some([0xff; 32]));
        assert_eq!(wide_int_le_bytes("-2", true), Some(le(&[0xfe], 0xff)));
        assert_eq!(wide_int_le_bytes("-0", true), Some([0; 32]));
        let mut max = [0xff; 32];
        max[31] = 0x7f;
        assert_eq!(wide_int_le_bytes(I256_MAX, true), Some(max));
        let mut min = [0; 32];
        min[31] = 0x80;
        assert_eq!(wide_int_le_bytes(I256_MIN, true), Some(min));
    }

    #[test]
    fn values_out_of_range_are_refused() {
        assert_eq!(wide_int_le_bytes(U256_MAX_PLUS_ONE, false), None);
        assert_eq!(wide_int_le_bytes("-1", false), None);
        // 2^255 and -(2^255 + 1) are one past each end of i256.
        let i256_max_plus_one =
            "57896044618658097711785492504343953926634992332820282019728792003956564819968";
        assert_eq!(wide_int_le_bytes(i256_max_plus_one, true), None);
        let i256_min_minus_one =
            "-57896044618658097711785492504343953926634992332820282019728792003956564819969";
        assert_eq!(wide_int_le_bytes(i256_min_minus_one, true), None);
        assert_eq!(wide_int_le_bytes(U256_MAX, true), None);
    }

    #[test]
    fn non_integers_are_refused() {
        for value in ["", "-", "0x10", "1.5", "12a", "+1"] {
            assert_eq!(wide_int_le_bytes(value, true), None, "{value:?}");
        }
    }
}
