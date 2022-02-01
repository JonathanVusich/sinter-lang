#[repr(u8)]
#[derive(PartialOrd, PartialEq, Eq, Debug)]
pub (crate) enum OpCode {
    Return,

    Pop,

    Push,

    GetConstant,

    SetLocal,

    GetLocal,

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

        assert_eq!(OpCode::Pop, OpCode::from(1));

        assert_eq!(OpCode::Push, OpCode::from(2));

        assert_eq!(OpCode::GetConstant, OpCode::from(3));

        assert_eq!(OpCode::SetLocal, OpCode::from(4));

        assert_eq!(OpCode::GetLocal, OpCode::from(5));

        assert_eq!(OpCode::Jump, OpCode::from(6));

        assert_eq!(OpCode::Loop, OpCode::from(7));

        assert_eq!(OpCode::Call, OpCode::from(8));
    }
}