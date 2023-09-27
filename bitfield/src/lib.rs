use std::ops::Range;

// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::bitfield;
pub use bitfield_impl::BitfieldSpecifier;
mod checks;
pub use checks::InRange;
pub use checks::MultipleOfEight;

use bitfield_impl::def_types;

const fn max_const(lhs: usize, rhs: usize) -> usize {
    if lhs > rhs {
        lhs
    } else {
        rhs
    }
}

const fn div_ceil(lhs: usize, rhs: usize) -> usize {
    let d = lhs / rhs;
    let r = lhs % rhs;
    if r > 0 && rhs > 0 {
        d + 1
    } else {
        d
    }
}

trait Constants<const TOTAL_BITS: usize, const BIT_START: usize> {
    type Target;
    const OFFSET: usize = BIT_START % 8;
    const LEN: usize = div_ceil(Self::OFFSET + TOTAL_BITS, 8);
    const START: usize = BIT_START / 8;
    const END: usize = Self::START + Self::LEN;
    const DROP_BITS: usize;
    const MASK: Self::Target;
    const ROTATE: usize;
    const EXTRA_MASK: u8;
}

trait GetSetterBits<const SIZE: usize> {
    type Target;
    fn get(values: &[u8; SIZE]) -> Self::Target;
    fn set(values: &mut [u8; SIZE], value: Self::Target);
    #[inline(always)]
    fn copy_bytes<const SRC_SIZE: usize, const DEST_SIZE: usize>(
        src: &[u8; SRC_SIZE],
        r: Range<usize>,
    ) -> [u8; DEST_SIZE] {
        let mut buffer = [0; DEST_SIZE];

        for (b, s) in buffer.iter_mut().zip(src[r].iter()) {
            *b = *s;
        }

        buffer
    }

    fn copy_bytes_from<const SRC_SIZE: usize, const DEST_SIZE: usize>(
        src: [u8; SRC_SIZE],
        dest: &mut [u8; DEST_SIZE],
        r: Range<usize>,
    ) {
        for (b, s) in dest[r].iter_mut().zip(src.iter()) {
            *b = *s;
        }
    }
}

pub trait Specifier {
    const BITS: usize;
    type Type;

    fn set<const BIT_START: usize, const SIZE: usize>(
        arr: &mut [u8; SIZE],
        num: <Self as Specifier>::Type,
    );
    fn get<const BIT_START: usize, const SIZE: usize>(
        arr: &[u8; SIZE],
    ) -> <Self as Specifier>::Type;
}

impl Specifier for bool {
    const BITS: usize = 1;

    type Type = bool;

    fn set<const BIT_START: usize, const SIZE: usize>(
        arr: &mut [u8; SIZE],
        num: <Self as Specifier>::Type,
    ) {
        <B1 as Specifier>::set::<BIT_START, SIZE>(arr, num as u8)
    }

    fn get<const BIT_START: usize, const SIZE: usize>(
        arr: &[u8; SIZE],
    ) -> <Self as Specifier>::Type {
        <B1 as Specifier>::get::<BIT_START, SIZE>(arr) == 1
    }
}

def_types!();
