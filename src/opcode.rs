#[repr(u8)]
#[derive(PartialOrd, PartialEq, Eq, Debug)]
pub (crate) enum OpCode {
    Return,
    GetConstant, // Must be followed by an u64 offset and u16 size.

    Pop,
    PopLarge,

    Push,
    PushLarge,

    SetLocal,
    GetLocal,

    GetGlobal, // Must be followed by a u64 offset and u16 size.
    SetGlobal, // Must be followed
    Jump,
    Loop,
    Call,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        let opcode: OpCode;
        unsafe {
            opcode = std::mem::transmute::<u8, OpCode>(byte);
        }
        opcode
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enum() {
        assert_eq!(OpCode::Return, OpCode::from(0));
        assert_eq!(OpCode::GetConstant, OpCode::from(1));
        assert_eq!(OpCode::Pop, OpCode::from(2));
    }
}