//! This is insane.
//!
//! Because Egglog commands are stateless (and Lattices are state!), we
//! have to create some plumbing for commands to access the Lattice structure.
//! This module contains that plumbing, and it's going to be a little unsafe.
//!
//! The gist of it is that we have a thread-local pointer to the currently active
//! Lattice structure. Whenever a `EgglogBackend` is created (a new `EGraph`),
//! we create a new Lattice and store a pointer to it in the thread-local storage.

use std::cell::RefCell;

use crate::{bubbler::backend::colors::Lattice, language::Language};

thread_local! {
    /// Pointer to the lattice associated with the currently active `EGraph`.
    ///
    /// Invariant:
    /// - The lattice is created together with its `EGraph`
    /// - The lattice outlives all egglog command invocations on that `EGraph`
    /// - The lattice is cleared when the `EGraph` is dropped
    /// - Commands execute synchronously on the same thread
    ///
    /// # Safety: (omg this is my first time writing one of these! ^.^)
    /// This is sound because:
    /// 1. The lattice pointer refers to a lattice associated with the active `EGraph`
    /// 2. The lattice outlives all command invocations on that `EGraph`
    /// 3. The pointer is cleared before the lattice is dropped
    /// 4. Commands execute synchronously on the same thread, so there are no data
    ///    races.
    static CURRENT_LATTICE: RefCell<Option<*const ()>> = const { RefCell::new(None) };
}

/// Set the lattice for the current backend.
///
/// # Panics
/// Screams if there is already a lattice set.
#[allow(dead_code)]
pub fn set_lattice<L: Language>(lattice: &Lattice<L>) {
    CURRENT_LATTICE.with(|slot| {
        let mut slot = slot.borrow_mut();
        assert!(slot.is_none(), "Lattice is already set for this thread");
        *slot = Some(lattice as *const Lattice<L> as *const ());
    });
}

/// Clears the lattice for the current backend.
///
/// This has to be called after the `EGraph` is dropped to avoid dangling pointers.
/// If you forget to call this, Bubbler will yell at ya. "Grumble!!"
#[allow(dead_code)]
pub fn clear_lattice() {
    CURRENT_LATTICE.with(|slot| {
        *slot.borrow_mut() = None;
    });
}

/// Access the current lattice for the backend.
///
/// # Panics
/// Panics if there is no lattice set for the current thread.
///
/// # Safety
/// This is safe because of the invariants on `CURRENT_LATTICE`.
#[allow(dead_code)]
pub fn with_lattice<L: Language, R, F: FnOnce(&Lattice<L>) -> R>(f: F) -> R {
    CURRENT_LATTICE.with(|slot| {
        let slot = slot.borrow();
        let ptr = slot.expect("Why is there no lattice set for this thread?");

        // SAFETY: See the safety comment on CURRENT_LATTICE.
        let lattice: &Lattice<L> = unsafe { &*(ptr as *const Lattice<L>) };

        f(lattice)
    })
}
