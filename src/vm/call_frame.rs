#[derive(Default, Copy, Clone)]
pub struct CallFrame {
    pub ip: usize,
    pub address: usize,
    pub size: usize,
}