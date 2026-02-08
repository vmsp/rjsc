use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use rjsc_sys::*;

/// A JavaScript runtime (context group).
///
/// Contexts created from the same runtime share a VM and
/// must remain on the same thread.
pub struct Runtime {
    inner: Rc<RuntimeInner>,
}

impl Runtime {
    /// Creates a new JavaScript runtime (context group).
    pub fn new() -> Self {
        let raw = unsafe { JSContextGroupCreate() };
        Runtime {
            inner: Rc::new(RuntimeInner {
                raw,
                tasks: RefCell::new(VecDeque::new()),
            }),
        }
    }

    pub(crate) fn raw(&self) -> JSContextGroupRef {
        self.inner.raw
    }

    /// Creates a new context inside this runtime.
    pub fn new_context(&self) -> crate::Context {
        crate::Context::new_in(self)
    }

    pub(crate) fn inner_clone(&self) -> Rc<RuntimeInner> {
        Rc::clone(&self.inner)
    }

    pub fn poll_async(&self, ctx: &crate::Context) -> usize {
        self.inner.poll_async(ctx)
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) struct RuntimeInner {
    raw: JSContextGroupRef,
    tasks: RefCell<VecDeque<crate::task::Task>>,
}

impl RuntimeInner {
    pub(crate) fn push_task(&self, task: crate::task::Task) {
        self.tasks.borrow_mut().push_back(task);
    }

    pub(crate) fn poll_async(&self, ctx: &crate::Context) -> usize {
        let mut pending = self.tasks.borrow_mut();
        let mut completed = 0usize;
        let mut remaining = VecDeque::new();

        while let Some(mut task) = pending.pop_front() {
            match task.poll(ctx) {
                std::task::Poll::Ready(()) => completed += 1,
                std::task::Poll::Pending => remaining.push_back(task),
            }
        }

        *pending = remaining;
        completed
    }
}

impl Drop for RuntimeInner {
    fn drop(&mut self) {
        unsafe { JSContextGroupRelease(self.raw) };
    }
}
