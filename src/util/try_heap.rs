use crate::xvalue::XResult;
use std::mem::{swap, ManuallyDrop};
use std::ptr;

struct Hole<'a, T: 'a> {
    data: &'a mut [T],
    elt: ManuallyDrop<T>,
    pos: usize,
}

impl<'a, T> Hole<'a, T> {
    /// Create a new `Hole` at index `pos`.
    ///
    /// Unsafe because pos must be within the data slice.
    #[inline]
    unsafe fn new(data: &'a mut [T], pos: usize) -> Self {
        debug_assert!(pos < data.len());
        // SAFE: pos should be inside the slice
        let elt = unsafe { ptr::read(data.get_unchecked(pos)) };
        Hole {
            data,
            elt: ManuallyDrop::new(elt),
            pos,
        }
    }

    #[inline]
    fn pos(&self) -> usize {
        self.pos
    }

    /// Returns a reference to the element removed.
    #[inline]
    fn element(&self) -> &T {
        &self.elt
    }

    /// Returns a reference to the element at `index`.
    ///
    /// Unsafe because index must be within the data slice and not equal to pos.
    #[inline]
    unsafe fn get(&self, index: usize) -> &T {
        debug_assert!(index != self.pos);
        debug_assert!(index < self.data.len());
        unsafe { self.data.get_unchecked(index) }
    }

    #[inline]
    unsafe fn move_to(&mut self, index: usize) {
        debug_assert!(index != self.pos);
        debug_assert!(index < self.data.len());
        unsafe {
            let ptr = self.data.as_mut_ptr();
            let index_ptr: *const _ = ptr.add(index);
            let hole_ptr = ptr.add(self.pos);
            ptr::copy_nonoverlapping(index_ptr, hole_ptr, 1);
        }
        self.pos = index;
    }
}

impl<T> Drop for Hole<'_, T> {
    #[inline]
    fn drop(&mut self) {
        // fill the hole again
        unsafe {
            let pos = self.pos;
            ptr::copy_nonoverlapping(&*self.elt, self.data.get_unchecked_mut(pos), 1);
        }
    }
}

pub(crate) struct TryHeap<T, F> {
    data: Vec<T>,
    is_le: F,
}

impl<T, W, F: FnMut(&T, &T) -> XResult<bool, W>> TryHeap<T, F> {
    #[must_use]
    pub fn with_capacity(capacity: usize, is_le: F) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            is_le,
        }
    }

    unsafe fn sift_up(&mut self, start: usize, pos: usize) -> XResult<usize, W> {
        // Take out the value at `pos` and create a hole.
        // SAFETY: The caller guarantees that pos < self.len()
        let mut hole = unsafe { Hole::new(&mut self.data, pos) };

        while hole.pos() > start {
            let parent = (hole.pos() - 1) / 2;

            // SAFETY: hole.pos() > start >= 0, which means hole.pos() > 0
            //  and so hole.pos() - 1 can't underflow.
            //  This guarantees that parent < hole.pos() so
            //  it's a valid index and also != hole.pos().
            let cmp = forward_err!((self.is_le)(hole.element(), unsafe { hole.get(parent) })?);
            if cmp {
                break;
            }

            // SAFETY: Same as above
            unsafe { hole.move_to(parent) };
        }

        Ok(Ok(hole.pos()))
    }

    unsafe fn sift_down_to_bottom(&mut self, mut pos: usize) -> XResult<(), W> {
        let end = self.len();
        let start = pos;

        // SAFETY: The caller guarantees that pos < self.len().
        let mut hole = unsafe { Hole::new(&mut self.data, pos) };
        let mut child = 2 * hole.pos() + 1;

        // Loop invariant: child == 2 * hole.pos() + 1.
        while child <= end.saturating_sub(2) {
            // SAFETY: child < end - 1 < self.len() and
            //  child + 1 < end <= self.len(), so they're valid indexes.
            //  child == 2 * hole.pos() + 1 != hole.pos() and
            //  child + 1 == 2 * hole.pos() + 2 != hole.pos().
            // FIXME: 2 * hole.pos() + 1 or 2 * hole.pos() + 2 could overflow
            //  if T is a ZST
            let rhs = unsafe { hole.get(child) };
            let lhs = unsafe { hole.get(child + 1) };
            let cmp = forward_err!((self.is_le)(rhs, lhs)?);
            child += cmp as usize;

            // SAFETY: Same as above
            unsafe { hole.move_to(child) };
            child = 2 * hole.pos() + 1;
        }

        if child == end - 1 {
            // SAFETY: child == end - 1 < self.len(), so it's a valid index
            //  and child == 2 * hole.pos() + 1 != hole.pos().
            unsafe { hole.move_to(child) };
        }
        pos = hole.pos();
        drop(hole);

        // SAFETY: pos is the position in the hole and was already proven
        //  to be a valid index.
        forward_err!(unsafe { self.sift_up(start, pos) }?);
        Ok(Ok(()))
    }

    pub(crate) fn len(&self) -> usize {
        self.data.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(crate) fn push(&mut self, item: T) -> XResult<(), W> {
        let old_len = self.len();
        self.data.push(item);
        // SAFETY: Since we pushed a new item it means that
        //  old_len = self.len() - 1 < self.len()
        forward_err!(unsafe { self.sift_up(0, old_len) }?);
        Ok(Ok(()))
    }

    pub(crate) fn pop(&mut self) -> XResult<Option<T>, W> {
        Ok(Ok(forward_err!(self
            .data
            .pop()
            .map(|mut item| {
                if !self.is_empty() {
                    swap(&mut item, &mut self.data[0]);
                    // SAFETY: !self.is_empty() means that self.len() > 0
                    forward_err!(unsafe { self.sift_down_to_bottom(0) }?);
                }
                Ok(Ok(item))
            })
            .transpose()?
            .transpose())))
    }
}
