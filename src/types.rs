//! Collection of small utility types.

// Should probably move all of the other Shrinkwrapped types in here as well.

use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap, PartialEq, Eq, Clone, Copy, Debug)]
#[shrinkwrap(mutable)]
pub struct Line(pub usize);
