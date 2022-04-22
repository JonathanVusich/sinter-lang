#[repr(u8)]
#[derive(PartialOrd, PartialEq, Eq, Debug)]
pub (crate) enum OpCode {
    ReturnVoid,
    Return,

    Pop,

    Add,
    Subtract,
    Multiply,
    Divide,

    Negate,

    GetConstant,

    SetLocal,

    GetLocal,

    Jump,
    JumpBack,

    Call,
}

impl From<u8> for OpCode {

    #[inline(always)]
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
        assert_eq!(OpCode::ReturnVoid, OpCode::from(0));

        assert_eq!(OpCode::Return, OpCode::from(1));

        assert_eq!(OpCode::Pop, OpCode::from(2));

        assert_eq!(OpCode::Add, OpCode::from(3));
        assert_eq!(OpCode::Subtract, OpCode::from(4));
        assert_eq!(OpCode::Multiply, OpCode::from(5));
        assert_eq!(OpCode::Divide, OpCode::from(6));

        assert_eq!(OpCode::Negate, OpCode::from(7));

        assert_eq!(OpCode::GetConstant, OpCode::from(8));
        assert_eq!(OpCode::SetLocal, OpCode::from(9));
        assert_eq!(OpCode::GetLocal, OpCode::from(10));
        assert_eq!(OpCode::Jump, OpCode::from(11));
        assert_eq!(OpCode::JumpBack, OpCode::from(12));
        assert_eq!(OpCode::Call, OpCode::from(13));
    }
}