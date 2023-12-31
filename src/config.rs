use std::sync::atomic::{AtomicBool, Ordering};

pub const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = FRAMES_MAX * 256;
pub const GC_HEAP_GROW_FACTOR: usize = 2;

pub struct GlobalFlag {
    value: AtomicBool,
}

impl GlobalFlag {
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

pub static TRACE_EXECUTION: GlobalFlag = GlobalFlag::new();
pub static PRINT_CODE: GlobalFlag = GlobalFlag::new();
pub static STRESS_GC: GlobalFlag = GlobalFlag::new();
pub static LOG_GC: GlobalFlag = GlobalFlag::new();
