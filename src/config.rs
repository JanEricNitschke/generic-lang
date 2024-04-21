use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

pub const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = FRAMES_MAX * 256;
pub const GC_HEAP_GROW_FACTOR: usize = 2;

pub struct GlobalBoolFlag {
    value: AtomicBool,
}

impl GlobalBoolFlag {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            value: AtomicBool::new(false),
        }
    }

    pub fn store(&self, value: bool) {
        self.value.store(value, Ordering::Relaxed);
    }

    pub fn load(&self) -> bool {
        self.value.load(Ordering::Relaxed)
    }
}

pub struct GlobalIntFlag {
    value: AtomicUsize,
}

impl GlobalIntFlag {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            value: AtomicUsize::new(0),
        }
    }

    pub fn store(&self, value: usize) {
        self.value.store(value, Ordering::Relaxed);
    }

    pub fn load(&self) -> usize {
        self.value.load(Ordering::Relaxed)
    }
}

pub static TRACE_EXECUTION: GlobalIntFlag = GlobalIntFlag::new();
pub static PRINT_CODE: GlobalBoolFlag = GlobalBoolFlag::new();
pub static STRESS_GC: GlobalBoolFlag = GlobalBoolFlag::new();
pub static LOG_GC: GlobalBoolFlag = GlobalBoolFlag::new();
